#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"

#include "souper/Infer/Preconditions.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Inst/InstGraph.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/GetSolver.h"
#include "souper/Util/DfaUtils.h"

using namespace llvm;
using namespace souper;

unsigned DebugLevel;

static cl::opt<unsigned, /*ExternalStorage=*/true>
DebugFlagParser("souper-debug-level",
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."),
     cl::location(DebugLevel), cl::init(1));

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input souper optimization>"),
              cl::init("-"));

static llvm::cl::opt<bool> RemoveLeaf("remove-leaf",
    llvm::cl::desc("Try to generalize a valid optimization by replacing"
                   "the use of a once-used variable with a new variable"
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> SymbolizeConstant("symbolize",
    llvm::cl::desc("Try to replace a concrete constant with a symbolic constant."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<size_t> SymbolizeNumInsts("symbolize-num-insts",
    llvm::cl::desc("Number of instructions to synthesize"
                   "(default=1)"),
    llvm::cl::init(1));

static llvm::cl::opt<bool> SymbolizeNoDFP("symbolize-no-dataflow",
    llvm::cl::desc("Do not generate optimizations with dataflow preconditions."),
    llvm::cl::init(false));

static llvm::cl::opt<bool> FixIt("fixit",
    llvm::cl::desc("Given an invalid optimization, generate a valid one."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> GeneralizeWidth("generalize-width",
    llvm::cl::desc("Given a valid optimization, generalize bitwidth."
                   "(default=false)"),
    llvm::cl::init(false));

static cl::opt<size_t> NumResults("generalization-num-results",
    cl::desc("Number of Generalization Results"),
    cl::init(5));

void Generalize(InstContext &IC, Solver *S, ParsedReplacement Input) {
  bool FoundWP = false;
  std::vector<std::map<Inst *, llvm::KnownBits>> KBResults;
  std::vector<std::map<Inst *, llvm::ConstantRange>> CRResults;
  S->abstractPrecondition(Input.BPCs, Input.PCs, Input.Mapping, IC, FoundWP, KBResults, CRResults);

  if (FoundWP && KBResults.empty() && CRResults.empty()) {
    Input.print(llvm::outs(), true);
  } else if (!KBResults.empty()) {
    for (auto &&Result : KBResults) { // Each result is a disjunction
      for (auto &Pair: Result) {
        Pair.first->KnownOnes = Pair.second.One;
        Pair.first->KnownZeros = Pair.second.Zero;
      }
      Input.print(llvm::outs(), true);
    }
  } else if (!CRResults.empty()) {
    for (auto &&Result : CRResults) { // Each result is a disjunction
      for (auto &Pair: Result) {
        Pair.first->Range = Pair.second;
      }
      Input.print(llvm::outs(), true);
    }
  }
}


void SymbolizeAndGeneralize(InstContext &IC, Solver *S, ParsedReplacement Input,
                            std::vector<Inst *> LHSConsts,
                            std::vector<Inst *> RHSConsts,
                            CandidateMap &Results) {
  std::map<Inst *, Inst *> InstCache;
  std::vector<Inst *> FakeConsts;
  for (size_t i = 0; i < LHSConsts.size(); ++i) {
    FakeConsts.push_back(
          IC.createVar(LHSConsts[i]->Width, "fakeconst_" + std::to_string(i)));
    InstCache[LHSConsts[i]] = FakeConsts[i];
  }

  // Does it makes sense for the expression to depend on other variables?
  // If yes, expand the third argument to include inputs
  EnumerativeSynthesis ES;
  auto Guesses = ES.generateExprs(IC, SymbolizeNumInsts, FakeConsts,
                                  RHSConsts[0]->Width);

  std::vector<std::vector<std::map<Inst *, llvm::KnownBits>>>
      Preconditions;

  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, APInt> ConstMap;
  auto LHS = getInstCopy(Input.Mapping.LHS, IC, InstCache,
                         BlockCache, &ConstMap, false);

  std::vector<Inst *> WithoutConsts;

  for (auto &Guess : Guesses) {
    std::set<Inst *> ConstSet;
    std::map <Inst *, llvm::APInt> ResultConstMap;
    souper::getConstants(Guess, ConstSet);
    if (!ConstSet.empty()) {
      std::map<Inst *, Inst *> InstCacheCopy = InstCache;
      InstCacheCopy[RHSConsts[0]] = Guess;
      auto RHS = getInstCopy(Input.Mapping.RHS, IC, InstCacheCopy,
                             BlockCache, &ConstMap, false);
      ConstantSynthesis CS;
      auto SMTSolver = GetUnderlyingSolver();
      auto EC = CS.synthesize(SMTSolver.get(), Input.BPCs, Input.PCs,
                           InstMapping (LHS, RHS), ConstSet,
                           ResultConstMap, IC, /*MaxTries=*/30, 10,
                           /*AvoidNops=*/true);
      if (!ResultConstMap.empty()) {
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        RHS = getInstCopy(RHS, IC, InstCache, BlockCache, &ResultConstMap, false);

        Results.push_back(CandidateReplacement(/*Origin=*/nullptr, InstMapping(LHS, RHS)));

      } else {
        if (DebugLevel > 2) {
          llvm::errs() << "Costant Synthesis ((no Dataflow Preconditions)) failed. \n";
        }
      }
    } else {
      WithoutConsts.push_back(Guess);
    }
  }
  std::swap(WithoutConsts, Guesses);

  for (auto &Guess : Guesses) {
    std::map<Inst *, Inst *> InstCacheCopy = InstCache;
    InstCacheCopy[RHSConsts[0]] = Guess;

    auto RHS = getInstCopy(Input.Mapping.RHS, IC, InstCacheCopy,
                           BlockCache, &ConstMap, false);

    std::vector<std::map<Inst *, llvm::KnownBits>> KBResults;
    std::vector<std::map<Inst *, llvm::ConstantRange>> CRResults;
    bool FoundWP = false;
    if (!SymbolizeNoDFP) {
      InstMapping Mapping(LHS, RHS);
      S->abstractPrecondition(Input.BPCs, Input.PCs, Mapping, IC, FoundWP, KBResults, CRResults);
    }
    Preconditions.push_back(KBResults);
    if (!FoundWP) {
      Guess = nullptr; // TODO: Better failure indicator
    } else {
      Guess = RHS;
    }
  }

  std::vector<size_t> Idx;
  std::vector<int> Utility;
  for (size_t i = 0; i < Guesses.size(); ++i) {
    Idx.push_back(i);
  }
  for (size_t i = 0; i < Preconditions.size(); ++i) {
    Utility.push_back(0);
    if (!Guesses[i]) continue;
    if (Preconditions[i].empty()) {
      Utility[i] = 1000; // High magic number
    }

    for (auto V : Preconditions[i]) {
      for (auto P : V) {
        auto W = P.second.getBitWidth();
        Utility[i] += (W - P.second.Zero.countPopulation());
        Utility[i] += (W - P.second.One.countPopulation());
      }
    }
  }

  std::sort(Idx.begin(), Idx.end(), [&Utility](size_t a, size_t b) {
    return Utility[a] > Utility[b];
  });

  for (size_t i = 0; i < Idx.size(); ++i) {
    if (Preconditions[Idx[i]].empty() && Guesses[Idx[i]]) {
      Results.push_back(CandidateReplacement(/*Origin=*/nullptr, InstMapping(LHS, Guesses[Idx[i]])));
    }
  }

  // if (!SymbolizeNoDFP) {
  //   for (size_t i = 0; i < std::min(Idx.size(), NumResults.getValue()); ++i) {
  //     for (auto Computed : Preconditions[Idx[i]]) {
  //       for (auto Pair : Computed) {
  //         Pair.first->KnownOnes = Pair.second.One;
  //         Pair.first->KnownZeros = Pair.second.Zero;
  //       }
  //       Results.push_back(CandidateReplacement(/*Origin=*/nullptr, InstMapping(LHS, Guesses[Idx[i]])));
  //     }
  //   }
  // }
}

void SymbolizeAndGeneralize(InstContext &IC,
                            Solver *S, ParsedReplacement Input) {
  std::vector<Inst *> LHSConsts, RHSConsts;
  auto Pred = [](Inst *I) {return I->K == Inst::Const;};
  findInsts(Input.Mapping.LHS, LHSConsts, Pred);
  findInsts(Input.Mapping.RHS, RHSConsts, Pred);

  CandidateMap Results;

  // One at a time
  for (auto LHSConst : LHSConsts) {
    SymbolizeAndGeneralize(IC, S, Input, {LHSConst}, RHSConsts, Results);
  }
  // TODO: Two at a time, etc. Is this replaceable by DFP?

  // All at once
  SymbolizeAndGeneralize(IC, S, Input, LHSConsts, RHSConsts, Results);

  // TODO: Move sorting here
  for (auto &&Result : Results) {
    Result.print(llvm::outs(), true);
    llvm::outs() << "\n";
  }
}

size_t InferWidth(Inst::Kind K, const std::vector<Inst *> &Ops) {
  switch (K) {
    case Inst::And:
    case Inst::Or:
    case Inst::Xor:
    case Inst::Sub:
    case Inst::Mul:
    case Inst::Add: return Ops[0]->Width;
    case Inst::Slt:
    case Inst::Sle:
    case Inst::Ult:
    case Inst::Ule: return 1;
    default: llvm_unreachable((std::string("Unimplemented ") + Inst::getKindName(K)).c_str());
  }
}

Inst *CloneInst(InstContext &IC, Inst *I, std::map<Inst *, size_t> &WidthMap) {
  if (I->K == Inst::Var) {
    return IC.createVar(WidthMap[I], I->Name); // TODO other attributes
  } else if (I->K == Inst::Const) {
    llvm_unreachable("Const");
  } else {
    std::vector<Inst *> Ops;
    for (auto Op : I->Ops) {
      Ops.push_back(CloneInst(IC, Op, WidthMap));
    }
    return IC.getInst(I->K, InferWidth(I->K, Ops), Ops);
  }
}

void GeneralizeBitWidth(InstContext &IC, Solver *S,
                     ParsedReplacement Input) {
  auto Vars = IC.getVariablesFor(Input.Mapping.LHS);

  assert(Vars.size() == 1 && "Multiple variables unimplemented.");

  std::map<Inst *, size_t> WidthMap;

  for (int i = 1; i < 64; ++i) {
    WidthMap[Vars[0]] = i;
    auto LHS = CloneInst(IC, Input.Mapping.LHS, WidthMap);
    auto RHS = CloneInst(IC, Input.Mapping.RHS, WidthMap);

    ReplacementContext RC;
    auto str = RC.printInst(LHS, llvm::outs(), true);
    llvm::outs() << "infer " << str << "\n";
    str = RC.printInst(RHS, llvm::outs(), true);
    llvm::outs() << "result " << str << "\n\n";
  }

}

// TODO: Return modified instructions instead of just printing out
void RemoveLeafAndGeneralize(InstContext &IC,
                               Solver *S, ParsedReplacement Input) {
  if (DebugLevel > 1) {
  llvm::errs() << "Attempting to generalize by removing leaf.\n";
  }
  // TODO: Do not generalize by removing leaf if LHS has one inst.

  std::map<Inst *, std::set<Inst *>> Uses;

  std::vector<Inst *> Stack{Input.Mapping.LHS, Input.Mapping.RHS};
  // TODO: Find uses in PCs/BPCs

  std::set<Inst *> Visited;
  while (!Stack.empty()) {
   auto Current = Stack.back();
   Stack.pop_back();
   Visited.insert(Current);

   for (auto Op : Current->Ops) {
     if (Op->K == Inst::Var) {
       Uses[Op].insert(Current);
       // Intentionally skips root
     }
     if (Visited.find(Op) == Visited.end()) {
       Stack.push_back(Op);
     }
   }
  }

  // Find a variable with one use;
  Inst *UsedOnce = nullptr;
  for (auto P : Uses) {
    if (P.second.size() == 1) {
      UsedOnce = P.first;
      break;
    }
  }

  if (!UsedOnce) {
    llvm::outs() << "Failed. No var with one use.";
    return;
  } else {
    Inst *User = *Uses[UsedOnce].begin();
    Inst *NewVar = IC.createVar(User->Width, "newvar");

    std::map<Inst *, Inst *> ICache;
    ICache[User] = NewVar;

    std::map<Block *, Block *> BCache;
    std::map<Inst *, llvm::APInt> CMap;

    Input.Mapping.LHS = getInstCopy(Input.Mapping.LHS, IC, ICache,
                                    BCache, &CMap, false);

    Input.Mapping.RHS = getInstCopy(Input.Mapping.RHS, IC, ICache,
                                    BCache, &CMap, false);

    // TODO: Replace PCs/BPCs
  }

  Generalize(IC, S, Input);
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);
  KVStore *KV = 0;

  std::unique_ptr<Solver> S = 0;
  S = GetSolver(KV);

  auto MB = MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!MB) {
    llvm::errs() << MB.getError().message() << '\n';
    return 1;
  }

  InstContext IC;
  std::string ErrStr;

  auto &&Data = (*MB)->getMemBufferRef();
  auto Inputs = ParseReplacements(IC, Data.getBufferIdentifier(),
                                  Data.getBuffer(), ErrStr);

  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  // TODO: Write default action which chooses what to do based on input structure

  for (auto &&Input: Inputs) {
    if (FixIt) {
      // TODO: Verify that inputs are valid optimizations
      Generalize(IC, S.get(), Input);
    }
    if (RemoveLeaf) {
      RemoveLeafAndGeneralize(IC, S.get(), Input);
    }
    if (SymbolizeConstant) {
      SymbolizeAndGeneralize(IC, S.get(), Input);
    }

    if (GeneralizeWidth) {
      GeneralizeBitWidth(IC, S.get(), Input);
    }
  }

  return 0;
}
