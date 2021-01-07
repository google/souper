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

static cl::opt<size_t> NumResults("generalization-num-results",
    cl::desc("Number of Generalization Results"),
    cl::init(5));


void Generalize(InstContext &IC, Solver *S, ParsedReplacement Input) {
  bool FoundWP = false;
  std::vector<std::map<Inst *, llvm::KnownBits>> Results;
  S->abstractPrecondition(Input.BPCs, Input.PCs, Input.Mapping, IC, FoundWP, Results);

  if (FoundWP && Results.empty()) {
    Input.print(llvm::outs(), true);
  } else {
    for (auto &&Result : Results) { // Each result is a disjunction
      for (auto Pair: Result) {
        Pair.first->KnownOnes = Pair.second.One;
        Pair.first->KnownZeros = Pair.second.Zero;
      }
      Input.print(llvm::outs(), true);
    }
  }
}

void SymbolizeAndGeneralize(InstContext &IC, Solver *S, ParsedReplacement Input,
                            std::vector<Inst *> LHSConsts,
                            std::vector<Inst *> RHSConsts) {
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
        ReplacementContext RC;
        auto LHSStr = RC.printInst(LHS, llvm::outs(), true);
        llvm::outs() << "infer " << LHSStr << "\n";
        auto RHSStr = RC.printInst(RHS, llvm::outs(), true);
        llvm::outs() << "result " << RHSStr << "\n\n";
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

    std::vector<std::map<Inst *, llvm::KnownBits>> Results;
    bool FoundWP = false;
    InstMapping Mapping(LHS, RHS);
    S->abstractPrecondition(Input.BPCs, Input.PCs, Mapping, IC, FoundWP, Results);

    Preconditions.push_back(Results);
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
  for (size_t i = 0; i < std::min(Idx.size(), NumResults.getValue()); ++i) {
    if (Preconditions[Idx[i]].empty() && Guesses[Idx[i]]) {
      ReplacementContext RC;
      auto LHSStr = RC.printInst(LHS, llvm::outs(), true);
      llvm::outs() << "infer " << LHSStr << "\n";
      auto RHSStr = RC.printInst(Guesses[Idx[i]], llvm::outs(), true);
      llvm::outs() << "result " << RHSStr << "\n\n";
    }
    if (SymbolizeNoDFP) {
      continue; // Do not print results with dataflow preconditions
    }
    for (auto Results : Preconditions[Idx[i]]) {
      for (auto Pair : Results) {
        Pair.first->KnownOnes = Pair.second.One;
        Pair.first->KnownZeros = Pair.second.Zero;
      }
      ReplacementContext RC;
      auto LHSStr = RC.printInst(LHS, llvm::outs(), true);
      llvm::outs() << "infer " << LHSStr << "\n";
      auto RHSStr = RC.printInst(Guesses[Idx[i]], llvm::outs(), true);
      llvm::outs() << "result " << RHSStr << "\n\n";
    }
  }
}

void SymbolizeAndGeneralize(InstContext &IC,
                            Solver *S, ParsedReplacement Input) {
  std::vector<Inst *> LHSConsts, RHSConsts;
  auto Pred = [](Inst *I) {return I->K == Inst::Const;};
  findInsts(Input.Mapping.LHS, LHSConsts, Pred);
  findInsts(Input.Mapping.RHS, RHSConsts, Pred);

  // One at a time
  for (auto LHSConst : LHSConsts) {
    SymbolizeAndGeneralize(IC, S, Input, {LHSConst}, RHSConsts);
  }
  // TODO: Two at a time, etc. Is this replaceable by DFP?

  // All at once
  SymbolizeAndGeneralize(IC, S, Input, LHSConsts, RHSConsts);
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
    // if (EviscerateRoot) {...}
    if (SymbolizeConstant) {
      SymbolizeAndGeneralize(IC, S.get(), Input);
    }
    // if (LiberateWidth) {...}
  }

  return 0;
}
