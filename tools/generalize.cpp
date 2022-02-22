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

static llvm::cl::opt<bool> Reduce("reduce",
    llvm::cl::desc("Try to reduce the number of instructions by replacing instructions with variables."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> ReduceKBIFY("reduce-kbify",
    llvm::cl::desc("Try to reduce the number of instructions by introducing known bits constraints."
                   "(default=false)"),
    llvm::cl::init(true));


static llvm::cl::opt<bool> ReducePrintAll("reduce-all-results",
    llvm::cl::desc("Print all reduced results."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> SymbolizeConstant("symbolize",
    llvm::cl::desc("Try to replace a concrete constant with a symbolic constant."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> SymbolizeWidth("symbolize-width",
    llvm::cl::desc("Try to replace a concrete constant with a function of bitwidth."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<size_t> SymbolizeNumInsts("symbolize-num-insts",
    llvm::cl::desc("Number of instructions to synthesize"
                   "(default=1)"),
    llvm::cl::init(1));

static llvm::cl::opt<bool> SymbolizeNoDFP("symbolize-no-dataflow",
    llvm::cl::desc("Do not generate optimizations with dataflow preconditions."),
    llvm::cl::init(false));

static llvm::cl::opt<bool> SymbolizeSimpleDF("symbolize-simple-dataflow",
    llvm::cl::desc("Generate simple dataflow facts supported by LLVM."),
    llvm::cl::init(false));

static llvm::cl::opt<bool> SymbolizeConstSynthesis("symbolize-constant-synthesis",
    llvm::cl::desc("Allow concrete constants in the generated code."),
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

  auto Components = FakeConsts;
  // Does it makes sense for the expression to depend on other variables?
  // If yes, expand the third argument to include inputs
  if (SymbolizeWidth) {
    std::vector<Inst *> Vars;
    findVars(Input.Mapping.LHS, Vars);
    for (auto &&V : Vars) {
      auto Width = IC.getInst(Inst::BitWidth, FakeConsts[0]->Width, {V});
      Components.push_back(Width);
      Components.push_back(IC.getInst(Inst::LogB, FakeConsts[0]->Width, {Width}));
      // auto IntMax = 1 << Width - 1
      // TODO other comps, like INT_MAX
      // Dedup widths which can not be different
    }
  }

  EnumerativeSynthesis ES;
  auto Guesses = ES.generateExprs(IC, SymbolizeNumInsts, Components,
                                  RHSConsts[0]->Width);

  std::vector<std::vector<std::map<Inst *, llvm::KnownBits>>>
      Preconditions;

  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, APInt> ConstMap;
  auto LHS = getInstCopy(Input.Mapping.LHS, IC, InstCache,
                         BlockCache, &ConstMap, false);

  std::vector<Inst *> WithoutConsts;

  auto TryConstSynth = [&](Inst *Guess, std::set<Inst *> &ConstSet) {
    std::map <Inst *, llvm::APInt> ResultConstMap;
    std::map<Inst *, Inst *> InstCacheCopy/* = InstCache*/;
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
      auto LHSCopy = getInstCopy(LHS, IC, InstCache, BlockCache, &ResultConstMap, true);
      RHS = getInstCopy(RHS, IC, InstCache, BlockCache, &ResultConstMap, true);

      Results.push_back(CandidateReplacement(/*Origin=*/nullptr, InstMapping(LHSCopy, RHS)));
      return true;
    } else {
      if (DebugLevel > 2) {
        llvm::errs() << "Constant Synthesis ((no Dataflow Preconditions)) failed. \n";
      }
    }
    return false;
  };


  for (auto &Guess : Guesses) {
    std::set<Inst *> ConstSet;
    souper::getConstants(Guess, ConstSet);
    if (!ConstSet.empty()) {
      if (SymbolizeConstSynthesis) {
        bool Success = TryConstSynth(Guess, ConstSet);
  //      llvm::errs() << "Succ:" << Success << "\n";
        if (SymbolizeSimpleDF) {
          if (!Success) {
            FakeConsts[0]->PowOfTwo = true;
            Success = TryConstSynth(Guess, ConstSet);
            FakeConsts[0]->PowOfTwo = false;
          }
    //      llvm::errs() << "Succ:" << Success << "\n";

          if (!Success) {
            FakeConsts[0]->NonZero = true;
            Success = TryConstSynth(Guess, ConstSet);
            FakeConsts[0]->NonZero = false;
          }
    //      llvm::errs() << "Succ:" << Success << "\n";

          if (!Success) {
            FakeConsts[0]->NonNegative = true;
            Success = TryConstSynth(Guess, ConstSet);
            FakeConsts[0]->NonNegative = false;
          }
    //      llvm::errs() << "Succ:" << Success << "\n";

          if (!Success) {
            FakeConsts[0]->Negative = true;
            Success = TryConstSynth(Guess, ConstSet);
            FakeConsts[0]->Negative = false;
          }
    //      llvm::errs() << "Succ:" << Success << "\n";
          (void)Success;
        }
      }

    } else {
      WithoutConsts.push_back(Guess);
    }
  }
  std::swap(WithoutConsts, Guesses);

  for (auto &Guess : Guesses) {
    std::map<Inst *, Inst *> InstCacheCopy/* = InstCache*/;
    InstCacheCopy[RHSConsts[0]] = Guess;



    auto RHS = getInstCopy(Input.Mapping.RHS, IC, InstCacheCopy,
                           BlockCache, &ConstMap, false);


    if (DebugLevel > 4) {
      {
      llvm::errs() << "GUESS:\n";
      ReplacementContext RC;
      RC.printInst(Guess, llvm::errs(), true);
      }
      {
      llvm::errs() << "JoinedGUESS:\n";
      ReplacementContext RC;
      RC.printInst(RHS, llvm::errs(), true);
      }
    }

    InstMapping Mapping(LHS, RHS);
    if (true /*remove when the other branch exists*/ || SymbolizeNoDFP) {
      bool IsValid;
      auto CheckAndSave = [&](){
        std::vector<std::pair<Inst *, APInt>> Models;
        if (auto EC = S->isValid(IC, Input.BPCs, Input.PCs, Mapping, IsValid, &Models)) {
          llvm::errs() << EC.message() << '\n';
        }
        if (IsValid) {
          InstMapping Clone;
          std::map<Inst *, Inst *> InstCache;
          std::map<Block *, Block *> BlockCache;
          std::map<Inst *, llvm::APInt> ConstMap;
          Clone.LHS = getInstCopy(Mapping.LHS, IC, InstCache, BlockCache, &ConstMap, true, false);
          Clone.RHS = getInstCopy(Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, true, false);
          Results.push_back(CandidateReplacement(/*Origin=*/nullptr, Clone));
        }
      };
      CheckAndSave();
      if (SymbolizeSimpleDF) {
        if (!IsValid) {
          FakeConsts[0]->PowOfTwo = true;
          CheckAndSave();
          FakeConsts[0]->PowOfTwo = false;
        }
        if (!IsValid) {
          FakeConsts[0]->NonZero = true;
          CheckAndSave();
          FakeConsts[0]->NonZero = false;
        }
        if (!IsValid) {
          FakeConsts[0]->NonNegative = true;
          CheckAndSave();
          FakeConsts[0]->NonNegative = false;
        }
        if (!IsValid) {
          FakeConsts[0]->Negative = true;
          CheckAndSave();
          FakeConsts[0]->Negative = false;
        }
      }

    } else {
//      std::vector<std::map<Inst *, llvm::KnownBits>> KBResults;
//      std::vector<std::map<Inst *, llvm::ConstantRange>> CRResults;
//      bool FoundWP = false;
//      S->abstractPrecondition(Input.BPCs, Input.PCs, Mapping, IC, FoundWP, KBResults, CRResults);
//      Preconditions.push_back(KBResults);
//      if (!FoundWP) {
//        Guess = nullptr; // TODO: Better failure indicator
//      } else {
//        Guess = RHS;
//      }
    }
  }

//  std::vector<size_t> Idx;
//  std::vector<int> Utility;
//  for (size_t i = 0; i < Guesses.size(); ++i) {
//    Idx.push_back(i);
//  }
//  for (size_t i = 0; i < Preconditions.size(); ++i) {
//    Utility.push_back(0);
//    if (!Guesses[i]) continue;
//    if (Preconditions[i].empty()) {
//      Utility[i] = 1000; // High magic number
//    }

//    for (auto V : Preconditions[i]) {
//      for (auto P : V) {
//        auto W = P.second.getBitWidth();
//        Utility[i] += (W - P.second.Zero.countPopulation());
//        Utility[i] += (W - P.second.One.countPopulation());
//      }
//    }
//  }

//  std::sort(Idx.begin(), Idx.end(), [&Utility](size_t a, size_t b) {
//    return Utility[a] > Utility[b];
//  });

//  for (size_t i = 0; i < Idx.size(); ++i) {
//    if (Preconditions[Idx[i]].empty() && Guesses[Idx[i]]) {
//      Results.push_back(CandidateReplacement(/*Origin=*/nullptr, InstMapping(LHS, Guesses[Idx[i]])));
//    }
//  }

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

  if (RHSConsts.empty()) {
    return;
    // TODO: Possible to just generalize LHS consts with preconditions?
  }

  CandidateMap Results;

//  // One at a time
//  for (auto LHSConst : LHSConsts) {
//    SymbolizeAndGeneralize(IC, S, Input, {LHSConst}, RHSConsts, Results);
//  }

  // TODO: Why does one at a time get solutions that all at once doesn't?


//   All at once
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

void collectInsts(Inst *I, std::set<Inst *> &Results) {
  std::vector<Inst *> Stack{I};
  while (!Stack.empty()) {
    auto Current = Stack.back();
    Stack.pop_back();

    Results.insert(Current);

    for (auto Child : Current->Ops) {
      if (Results.find(Child) == Results.end()) {
        Stack.push_back(Child);
      }
    }
  }
}

class Reducer {
public:
  Reducer(InstContext &IC_, Solver *S_) : IC(IC_), S(S_), varnum(0), numSolverCalls(0) {}

  ParsedReplacement ReduceGreedy(ParsedReplacement Input) {
    std::set<Inst *> Insts;
    collectInsts(Input.Mapping.LHS, Insts);
    // TODO: topological sort, to reduce number of solver calls
    // Try to remove one instruction at a time
    int failcount = 0;
    std::set<Inst *> Visited;
    do {
      auto It = Insts.begin();
      auto I = *It;
      Insts.erase(It);
      if (Visited.find(I) != Visited.end()) {
        continue;
      }
      Visited.insert(I);
      if (!safeToRemove(I, Input)) {
        continue;
      }
      auto Copy = Input;
      Eliminate(Input, I);
      if (!Verify(Input)) {
        Input = Copy;
        failcount++;
        if (failcount >= Insts.size()) {
          break;
        }
        continue;
      }
      Insts.clear();
      collectInsts(Input.Mapping.LHS, Insts);
    } while (!Insts.empty());
    return Input;
  }

  ParsedReplacement ReduceGreedyKBIFY(ParsedReplacement Input) {
    std::set<Inst *> Insts;
    collectInsts(Input.Mapping.LHS, Insts);
    // TODO: topological sort, to reduce number of solver calls
    // Try to remove one instruction at a time
    int failcount = 0;
    std::set<Inst *> Visited;
    do {
      auto It = Insts.begin();
      auto I = *It;
      Insts.erase(It);
      if (Visited.find(I) != Visited.end()) {
        continue;
      }
      Visited.insert(I);
      if (!safeToRemove(I, Input) || I->Width == 1 /*1 bit KB doesn't make sense*/) {
        continue;
      }
      auto Copy = Input;
      auto NewVar = Eliminate(Input, I);

      // Input won't verify because ReduceGreedy has been called before this.

      // Try to replace NewVar with a symbolic constant and do constant synthesis.
      Inst *C = IC.createSynthesisConstant(NewVar->Width, 0);
      std::map<Inst *, Inst *> InstCache = {{NewVar, C}};
      std::map<Block *, Block *> BlockCache;
      std::map<Inst *, llvm::APInt> ConstMap;

      InstMapping Rep;
      Rep.LHS = getInstCopy(Input.Mapping.LHS, IC, InstCache, BlockCache, &ConstMap, false, false);
      Rep.RHS = getInstCopy(Input.Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, false, false);

      ConstantSynthesis CS;
      std::set<Inst *> ConstSet{C};

//      llvm::errs() << "Constant synthesis problem: \n";
//      Input.print(llvm::errs(), true);
//      llvm::errs() << "....end.... \n";

      if (auto EC = CS.synthesize(S->getSMTLIBSolver(), Input.BPCs, Input.PCs,
                                  Rep, ConstSet, ConstMap, IC, 30, 60, false)) {
        llvm::errs() << "Constant Synthesis internal error : " <<  EC.message();
      }

      if (ConstMap.empty()) {
//        if (DebugLevel > 3) {
          llvm::errs() << "Constant Synthesis failed, moving on.\n";
//        }
        Input = Copy;
        failcount++;
        if (failcount >= Insts.size()) {
          break;
        }
      } else {
        InstCache.clear();
        InstCache[C] = NewVar;

        NewVar->KnownOnes = ConstMap[C];
        NewVar->KnownZeros = ~ConstMap[C];

        // Give up if can't be weakened 'too much'
        const size_t WeakeningThreshold = NewVar->Width/2;
        size_t BitsWeakened = 0;

        for (size_t i = 0; i < NewVar->Width; ++i) {
          auto SaveZero = NewVar->KnownZeros;
          auto SaveOne = NewVar->KnownOnes;

          NewVar->KnownZeros.clearBit(i);
          NewVar->KnownOnes.clearBit(i);

          if (!Verify(Input)) {
            NewVar->KnownZeros = SaveZero;
            NewVar->KnownOnes = SaveOne;
          } else {
            BitsWeakened++;
          }
        }
        if (BitsWeakened < WeakeningThreshold) {
          Input = Copy;
          failcount++;
          if (failcount >= Insts.size()) {
            break;
          }
        }

      }
      Insts.clear();
      collectInsts(Input.Mapping.LHS, Insts);
    } while (!Insts.empty());
    return Input;
  }


  // Assumes Input is valid
  ParsedReplacement ReducePCs(ParsedReplacement Input) {

    std::set<size_t> UnnecessaryPCs;
    for (size_t i =0; i < Input.PCs.size(); ++i) {
      std::vector<InstMapping> PCsExceptOne;
      for (size_t j = 0; j < Input.PCs.size(); ++j) {
        if (i != j) {
          PCsExceptOne.push_back(Input.PCs[i]);
        }
      }

      std::vector<std::pair<Inst *, APInt>> Models;
      bool Valid;
      if (std::error_code EC = S->isValid(IC, Input.BPCs, PCsExceptOne, Input.Mapping, Valid, &Models)) {
        llvm::errs() << EC.message() << '\n';
      }
      if (Valid) {
        UnnecessaryPCs.insert(i);
      }
    }

    std::vector<InstMapping> NecessaryPCs;
    for (size_t i =0; i < Input.PCs.size(); ++i) {
      if (UnnecessaryPCs.find(i) == UnnecessaryPCs.end()) {
        NecessaryPCs.push_back(Input.PCs[i]);
      }
    }

    auto Result = Input;
    Result.PCs = NecessaryPCs;
    return Result;
  }

  // Assumes Input is valid
  ParsedReplacement WeakenKB(ParsedReplacement Input) {
    std::vector<Inst *> Vars;
    findVars(Input.Mapping.LHS, Vars);
    for (auto &&V : Vars) {
      auto OriZero = V->KnownZeros;
      auto OriOne = V->KnownOnes;
      if (OriZero == 0 && OriOne == 0) {
        continue; // this var doesn't have a knownbits condition
      }
      if (OriZero.getBitWidth() != V->Width  || OriOne.getBitWidth() != V->Width) {
        continue; // this var doesn't have a well formed knownbits condition
      }

      // Try to remove KB
      V->KnownOnes = llvm::APInt(V->Width, 0);
      V->KnownZeros = llvm::APInt(V->Width, 0);
      if (Verify(Input)) {
        continue; // Removed KB from this var
      }
      V->KnownOnes = OriOne;
      V->KnownZeros = OriZero;

      // Try resetting bitwise KB

      for (size_t i = 0; i < V->Width; ++i) {
        auto Ones = V->KnownOnes;
        if (Ones[i]) {
          V->KnownOnes.setBitVal(i, false);
          if (!Verify(Input)) {
            V->KnownOnes = Ones;
          }
        }
        auto Zeros = V->KnownZeros;
        if (Zeros[i]) {
          V->KnownZeros.setBitVal(i, false);
          if (!Verify(Input)) {
            V->KnownZeros = Zeros;
          }
        }
      }
    }
    return Input;
  }

  // Assumes Input is valid
  ParsedReplacement WeakenCR(ParsedReplacement Input) {
    // Just try to remove CR for now.
    std::vector<Inst *> Vars;
    findVars(Input.Mapping.LHS, Vars);

    for (auto &&V : Vars) {
      auto Ori = V->Range;
      if (V->Range.isFullSet()) {
        continue;
      }
      V->Range = llvm::ConstantRange(V->Width, true);
      if (!Verify(Input)) {
        V->Range = Ori;
      }
      // TODO: Try Widening Range
    }

    return Input;
  }

  // Assumes Input is valid
  ParsedReplacement WeakenDB(ParsedReplacement Input) {
    auto Ori = Input.Mapping.LHS->DemandedBits;
    auto Width = Input.Mapping.LHS->Width;
    if (Ori.getBitWidth() != Width || Ori.isAllOnesValue()) {
      return Input;
    }
    // Try replacing with all ones.
    Input.Mapping.LHS->DemandedBits.setAllBits();
    if (Verify(Input)) {
      return Input;
    }
    Input.Mapping.LHS->DemandedBits = Ori;

    for (size_t i = 0; i < Width; ++i) {
      auto Last = Input.Mapping.LHS->DemandedBits;
      if (!Last[i]) {
        Input.Mapping.LHS->DemandedBits.setBitVal(i, true);
        if (!Verify(Input)) {
          Input.Mapping.LHS->DemandedBits = Last;
        }
      }
    }

    return Input;
  }

  bool Verify(ParsedReplacement &Input) {
    std::vector<std::pair<Inst *, APInt>> Models;
    bool Valid;
    if (std::error_code EC = S->isValid(IC, Input.BPCs, Input.PCs, Input.Mapping, Valid, &Models)) {
      llvm::errs() << EC.message() << '\n';
    }
    numSolverCalls++;
    return Valid;
  }

  bool safeToRemove(Inst *I, ParsedReplacement &Input) {
    if (I == Input.Mapping.LHS || I == Input.Mapping.RHS || I->K == Inst::Var || I->K == Inst::Const ||
        I->K == Inst::UMulWithOverflow || I->K == Inst::UMulO ||
        I->K == Inst::SMulWithOverflow || I->K == Inst::SMulO ||
        I->K == Inst::UAddWithOverflow || I->K == Inst::UAddO ||
        I->K == Inst::SAddWithOverflow || I->K == Inst::SAddO ||
        I->K == Inst::USubWithOverflow || I->K == Inst::USubO ||
        I->K == Inst::SSubWithOverflow || I->K == Inst::SSubO) {
      return false;
    }
    return true;
  }

  Inst *Eliminate(ParsedReplacement &Input, Inst *I) {
    // Try to replace I with a new Var.
    Inst *NewVar = IC.createVar(I->Width, "newvar" + std::to_string(varnum++));

    std::map<Inst *, Inst *> ICache;
    ICache[I] = NewVar;

    std::map<Block *, Block *> BCache;
    std::map<Inst *, llvm::APInt> CMap;

    ParsedReplacement NewInst = Input;

    Input.Mapping.LHS = getInstCopy(Input.Mapping.LHS, IC, ICache,
                                    BCache, &CMap, false);

    Input.Mapping.RHS = getInstCopy(Input.Mapping.RHS, IC, ICache,
                                    BCache, &CMap, false);

    for (auto &M : Input.PCs) {
      M.LHS = getInstCopy(M.LHS, IC, ICache, BCache, &CMap, false);
      M.RHS = getInstCopy(M.RHS, IC, ICache, BCache, &CMap, false);
    }
    for (auto &BPC : Input.BPCs) {
      BPC.PC.LHS = getInstCopy(BPC.PC.LHS, IC, ICache, BCache, &CMap, false);
      BPC.PC.RHS = getInstCopy(BPC.PC.RHS, IC, ICache, BCache, &CMap, false);
    }
    return NewVar;
  }

  void ReduceRec(ParsedReplacement Input_, std::vector<ParsedReplacement> &Results) {

    // Try to remove subsets of instructions recursively, and store all valid results
    ReplacementContext RC;
    std::string Str;
    llvm::raw_string_ostream SStr(Str);
    RC.printInst(Input_.Mapping.LHS, SStr, false);

    Inst *Ante = IC.getConst(llvm::APInt(1, true));
    for (auto PC : Input_.PCs ) {
      Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
      Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
    }

    RC.printInst(Ante, SStr, false);
    SStr.flush();

  //  llvm::errs() << Str << "\n";

  //  auto Str = Input_.getString(false);
    if (DNR.find(Str) != DNR.end()) {
      return;
    } else {
      DNR.insert(Str);
    }

    std::set<Inst *> Insts;
    collectInsts(Input_.Mapping.LHS, Insts);
    collectInsts(Input_.Mapping.RHS, Insts);

  //  for (auto &&PC : Input_.PCs) {
  //    collectInsts(PC.LHS, Insts);
  //    collectInsts(PC.RHS, Insts);
  //  }

  //  for (auto &&BPC : Input_.BPCs) {
  //    collectInsts(BPC.PC.LHS, Insts);
  //    collectInsts(BPC.PC.RHS, Insts);
  //  }

    if (Insts.size() <= 1) {
      return; // Base case
    }

    // Remove at least one instruction and call recursively for valid opts
    for (auto I : Insts) {
      ParsedReplacement Input = Input_;

      if (!safeToRemove(I, Input)) {
        continue;
      }

      Eliminate(Input, I);

      if (Verify(Input)) {
        Results.push_back(Input);
        ReduceRec(Input, Results);
      } else {
        if (DebugLevel >= 2) {
          llvm::outs() << "Invalid attempt.\n";
          Input.print(llvm::outs(), true);
        }
      }
    }
  }
  void Stats() {
    llvm::outs() << "Solver Calls: " << numSolverCalls << "\n";
  }
private:
  InstContext &IC;
  Solver *S;
  int varnum;
  int numSolverCalls;
  std::unordered_set<std::string> DNR;
};

void ReduceAndGeneralize(InstContext &IC,
                               Solver *S, ParsedReplacement Input) {
  std::vector<std::pair<Inst *, APInt>> Models;
  bool Valid;
  if (std::error_code EC = S->isValid(IC, Input.BPCs, Input.PCs, Input.Mapping, Valid, &Models)) {
    llvm::errs() << EC.message() << '\n';
  }
  if (!Valid) {
    llvm::errs() << "Invalid Input.\n";
    return;
  }

  Reducer R(IC, S);

  std::vector<ParsedReplacement> Results;
  Input = R.ReducePCs(Input);
  Input = R.ReduceGreedy(Input);
  Input = R.WeakenKB(Input);
  Input = R.WeakenCR(Input);
  Input = R.WeakenDB(Input);

  if (ReduceKBIFY) {
    Input = R.ReduceGreedyKBIFY(Input);
  }

//  Results.push_back(Input);

  R.ReduceRec(Input, Results);
  if (DebugLevel > 3) {
    R.Stats();
  }
  if (!Results.empty()) {
    std::set<std::string> DedupedResults;
    for (auto &&Result : Results) {
      DedupedResults.insert(Result.getString(false));
    }

    std::vector<std::string> SortedResults(DedupedResults.begin(), DedupedResults.end());
    std::sort(SortedResults.begin(), SortedResults.end(), [](auto a, auto b){return a.length() < b.length();});

    for (auto &&S : SortedResults) {
      if (DebugLevel > 2) {
        llvm::outs() << "\n\nResult:\n";
      }
      llvm::outs() << S << '\n';
      if (!ReducePrintAll) {
        break;
      }
    }
  } else {
    Input.print(llvm::outs(), true);
    if (DebugLevel > 2) {
      llvm::errs() << "Failed to Generalize.\n";
    }
  }
  if (DebugLevel > 2) {
    llvm::outs() << "Number of Results: " << Results.size() << ".\n";
  }
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
    if (Reduce) {
      ReduceAndGeneralize(IC, S.get(), Input);
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
