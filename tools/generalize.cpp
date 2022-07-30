#define _LIBCPP_DISABLE_DEPRECATION_WARNINGS

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"

#include "souper/Infer/Preconditions.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/Pruning.h"
#include "souper/Infer/SynthUtils.h"
#include "souper/Inst/InstGraph.h"
#include "souper/Parser/Parser.h"
#include "souper/Generalize/Reducer.h"
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
    llvm::cl::init(true));

static llvm::cl::opt<bool> SymbolizeKBDF("symbolize-infer-kb",
    llvm::cl::desc("Generate KB constraints for symbolic constants."),
    llvm::cl::init(true));

static llvm::cl::opt<bool> SymbolizeConstSynthesis("symbolize-constant-synthesis",
    llvm::cl::desc("Allow concrete constants in the generated code."),
    llvm::cl::init(true));

static llvm::cl::opt<bool> SymbolizeHackersDelight("symbolize-bit-hacks",
    llvm::cl::desc("Include bit hacks in the components."),
    llvm::cl::init(true));

static llvm::cl::opt<bool> FixIt("fixit",
    llvm::cl::desc("Given an invalid optimization, generate a valid one."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> GeneralizeWidth("all-widths",
    llvm::cl::desc("Given a valid optimization, output all bitwidths."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> GeneralizeWidthVerify("all-widths-verify",
    llvm::cl::desc("Verify for all bitwidths"
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> MinimizeWidth("minimize-width",
    llvm::cl::desc("Find valid version with minimal width"
                   "(default=false)"),
    llvm::cl::init(false));

static cl::opt<size_t> NumResults("generalization-num-results",
    cl::desc("Number of Generalization Results"),
    cl::init(5));

static cl::opt<bool> Everything("everything",
    cl::desc("Run everything, output one result."),
    cl::init(false));

static cl::opt<bool> SymbolicDF("symbolic-df",
    cl::desc("Generalize with symbolic dataflow facts."),
    cl::init(false));

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


// This can probably be done more efficiently, but likely not the bottleneck anywhere
std::vector<std::vector<int>> GetCombinations(std::vector<int> Counts) {
  if (Counts.size() == 1) {
    std::vector<std::vector<int>> Result;
    for (int i = 0; i < Counts[0]; ++i) {
      Result.push_back({i});
    }
    return Result;
  }

  auto Last = Counts.back();
  Counts.pop_back();
  auto Partial = GetCombinations(Counts);

  std::vector<std::vector<int>> Result;
  for (int i = 0; i < Last; ++i) {
    for (auto Copy : Partial) {
      Copy.push_back(i);
      Result.push_back(Copy);
    }
  }
  return Result;
}

void SymbolizeWidthNew(InstContext &IC, Solver *S, ParsedReplacement Input,
                       CandidateMap &Results) {
  std::vector<Inst *> Consts;
  std::vector<Inst *> Vars;
  findVars(Input.Mapping.LHS, Vars);

  auto Pred = [](Inst *I) {return I->K == Inst::Const;};
  findInsts(Input.Mapping.LHS, Consts, Pred);
  findInsts(Input.Mapping.RHS, Consts, Pred);

  for (auto &&C : Consts) {
    std::vector<Inst *> Components;
    Components.push_back(IC.getInst(Inst::BitWidth, C->Width, {Vars[0]}));

    EnumerativeSynthesis ES;
    auto Guesses = ES.generateExprs(IC, SymbolizeNumInsts, Components,
                                    C->Width);

    for (auto &&G : Guesses) {
      std::map<Inst *, Inst *> InstCache;
      InstCache[C] = G;
      int SymExprCount = 0;


      auto LHS = Replace(Input.Mapping.LHS, IC, InstCache);
      auto RHS = Replace(Input.Mapping.RHS, IC, InstCache);

      InstMapping Mapping(LHS, RHS);
      auto Copy = Input;
      Copy.Mapping = Mapping;
      bool IsValid = false;

      auto CheckAndSave = [&] () {
        auto Result = Verify(Copy, IC, S);
        if (Result.Mapping.LHS && Result.Mapping.RHS) {
          CandidateReplacement Rep(nullptr, Result.Mapping);
          Rep.PCs = Result.PCs;
          Results.push_back(Rep);
        }
      };

      CheckAndSave();

      // TODO Make preconditions consistent
      // if (SymbolizeSimpleDF) {
      //   for (auto &&C : SymCS) {
      //     if (!IsValid) {
      //       C->PowOfTwo = true;
      //       CheckAndSave();
      //       C->PowOfTwo = false;
      //     }
          // if (!IsValid) {
          //   C->NonZero = true;
          //   CheckAndSave();
          //   C->NonZero = false;
          // }
          // if (!IsValid) {
          //   C->NonNegative = true;
          //   CheckAndSave();
          //   C->NonNegative = false;
          // }
          // if (!IsValid) {
          //   C->Negative = true;
          //   CheckAndSave();
          //   C->Negative = false;
          // }
      //   }
      // }
      // TODO Is there a better way of doing this?
      // TODO Other kinds of preconditions?

    }

  }
}

void ReplaceConstsSimple(InstContext &IC, Solver *S,
                         ParsedReplacement Input,
                         std::vector<Inst *> LHSConsts,
                         std::vector<Inst *> RHSConsts,
                         CandidateMap &Results) {
  // FIXME Start from here
  // Try replacing each constant with a width independent
  // expr or a kb expr
}

template <typename C, typename F>
bool All(const C &c, F f) {
  for (auto &&m : c) {
    if (!f(m)) {
      return false;
    }
  }
  return true;
}



bool InferPreconditionsAndVerify(ParsedReplacement Input, CandidateMap &Results,
                                 std::vector<std::pair<Inst *, llvm::APInt>> &SymCS, InstContext &IC, Solver *S) {
  
  auto SOLVE = [&]() {
    auto Result = Verify(Input, IC, S);
    if (Result.Mapping.LHS && Result.Mapping.RHS) {
      CandidateReplacement Rep(nullptr, Result.Mapping);
      Rep.PCs = Result.PCs;
      Results.push_back(Rep);
      return true;
    } else {
      return false;
    }
  };
  
  if (SOLVE()) return true;
  
  std::vector<Inst *> Insts;
  findVars(Input.Mapping.LHS, Insts);
  
  std::set<Inst *> ModelInsts;
  for (auto I : SymCS) {
    ModelInsts.insert(I.first);
  }
  
  
  
  std::vector<std::map<Inst *, llvm::APInt>> Inputs;
  Inputs.push_back({});
  for (auto &&P : SymCS) {
    Inputs.back()[P.first] = P.second;
  }
  
  // TODO Is this worth using?
  bool SynthNewConsts = false;
  if (SynthNewConsts) {
    for (auto &&I : findValidConsts(Input, ModelInsts, IC, S, 5)) {
      Inputs.push_back(I);
    }
  }
  
  std::map<Inst *, std::vector<llvm::APInt>> CVals;
  
  for (auto &&I : Inputs) {
    for (auto &&P: I) {
      CVals[P.first].push_back(P.second);
    }
  }
  
  if (Inputs.empty()) {
    if (DebugLevel > 4)
      llvm::errs() << "Could not find valid concrete constants.\n";
    return false;
  }
  
  if (SymbolizeSimpleDF) {

#define DF(Fact, Check)                                    \
if (All(CVals[C], [](auto Val) { return Check;})) {        \
C->Fact = true; if (SOLVE()) return true; C->Fact = false;};

   for (auto &&P : SymCS) {
     auto C = P.first;
     DF(PowOfTwo, Val.isPowerOf2())
     DF(NonZero, Val != 0);
     DF(NonNegative, Val.uge(0));
     DF(Negative, Val.slt(0));
   }
#undef DF
  }

  if (SymbolizeKBDF) {
//    Reducer R(IC, S);
    
  }
  
  return false;
}


// FIXME Other interesting things to try
// Symbolic KB in preconditions
// Symbolic KB with extra constraints.
// Simple relational conditions for symcs
// Relations between symcs

// TODO Document options
void SymbolizeAndGeneralizeImpl(InstContext &IC, Solver *S, ParsedReplacement Input,
                            std::vector<Inst *> LHSConsts,
                            std::vector<Inst *> RHSConsts,
                            CandidateMap &Results) {
  
  if (RHSConsts.empty()) {
    // FIXME: Support generalizing LHS Consts too.
    return;
  }

  std::vector<std::pair<Inst *, llvm::APInt>> SymCS;
  std::vector<Inst *> Vars;
  std::vector<Inst *> SymDFVars;
  findVars(Input.Mapping.LHS, Vars);
  
  std::map<Inst *, Inst *> InstCache;
  ValueCache VC;
  // Create a symbolic const for each LHS const
  for (size_t i = 0; i < LHSConsts.size(); ++i) {
    auto C = IC.createVar(LHSConsts[i]->Width, "symconst_" + std::to_string(i));
    SymCS.push_back({C, LHSConsts[i]->Val});
    InstCache[LHSConsts[i]] = C;
    VC[C] = EvalValue(LHSConsts[i]->Val);
  }

  std::vector<Inst *> Components;
  
//  if (!SymbolizeConstSynthesis) {
//    std::set<Inst *> ConcreteConsts; // for deduplication

//    for (auto C : LHSConsts) {
//      ConcreteConsts.insert(C);
//      ConcreteConsts.insert(IC.getConst(llvm::APInt(C->Width, 1)));
//      ConcreteConsts.insert(IC.getConst(llvm::APInt(C->Width, -1)));
//      ConcreteConsts.insert(IC.getConst(llvm::APInt(C->Width, 2)));
//      ConcreteConsts.insert(IC.getConst(llvm::APInt(C->Width, 31)));
//    }
//    for (auto C : RHSConsts) {
//      ConcreteConsts.insert(C);
//    }
//    for (auto C : ConcreteConsts) {
//      Components.push_back(C);
//    }
//  }
  
//  // Symbolic known bits
//  if (false) {
//    for (size_t i = 0; i < Vars.size(); ++i) {
//      SymDFVars.push_back(IC.createVar(Vars[i]->Width, "symk_one_" + std::to_string(i)));
//      SymDFVars.back()->SymOneOf = Vars[i];
////      Vars[i]->SymKnownOnes = SymDFVars.back();
//      Components.push_back(SymDFVars.back());

//      SymDFVars.push_back(IC.createVar(Vars[i]->Width, "symk_zero_" + std::to_string(i)));
//      SymDFVars.back()->SymZeroOf = Vars[i];
////      Vars[i]->SymKnownZeros = SymDFVars.back();
//      Components.push_back(SymDFVars.back());
//    }
//  }


    // Put custom components here
  if (SymbolizeConstant) {
    for (auto C : SymCS) {
      // Minus One
      Components.push_back(Builder(C.first, IC).Sub(1)());
      // Flip bits
      Components.push_back(Builder(C.first, IC).Xor(-1)());
      
      // Custom test
      // auto M1 = IC.getConst(llvm::APInt(C->Width, -1));
      // auto Test = Builder(C, IC).Add(M1).Xor(M1).And(M1);
      // Components.push_back(Test());
//      auto M0 = IC.getConst(llvm::APInt(C->Width, 0));
//      auto Test = Builder(M0, IC).Sub(C);
//      Components.push_back(Test());
    }
  }

  for (auto SymC : SymCS) {
    Components.push_back(SymC.first);
  }

  // TODO Derive relations between LHSConsts and use them as preconditions

  // Must consider all targets at once
  // Test phase before verification
  // Is it possible to pre-generate the set of all possible constants? No.
  // Some? definitely.

  std::vector<std::vector<Inst *>> Candidates;

  ConcreteInterpreter CI(VC);
  for (auto &&Target : RHSConsts) {
    Candidates.push_back({});
    EnumerativeSynthesis ES;
    auto Guesses = ES.generateExprs(IC, SymbolizeNumInsts, Components,
                                    Target->Width);
    for (auto &&Guess : Guesses) {
      std::set<Inst *> ConstSet;
      souper::getConstants(Guess, ConstSet);
      if (!ConstSet.empty()) {
        if (SymbolizeConstSynthesis) {
          Candidates.back().push_back(Guess);
        }
      } else {
          Candidates.back().push_back(Guess);
      }
    }
  }

  std::vector<int> Counts;
  for (auto &&Cand : Candidates) {
    Counts.push_back(Cand.size());
  }

  // Generate all combination of candidates
  std::vector<std::vector<int>> Combinations = GetCombinations(Counts);

  for (auto &&Comb : Combinations) {
    int SymExprCount = 0;
    auto InstCacheRHS = InstCache;
    for (int i = 0; i < RHSConsts.size(); ++i) {
      InstCacheRHS[RHSConsts[i]] = Candidates[i][Comb[i]];
      if (Candidates[i][Comb[i]]->K != Inst::Var) {
        Candidates[i][Comb[i]]->Name = std::string("constexpr_") + std::to_string(SymExprCount++);
      }
    }

    auto LHS = Replace(Input.Mapping.LHS, IC, InstCache);
    auto RHS = Replace(Input.Mapping.RHS, IC, InstCacheRHS);

    InstMapping Mapping(LHS, RHS);
    auto Copy = Input;
    Copy.Mapping = Mapping;
    bool IsValid = false;
    
    static int n = 0;
    InferPreconditionsAndVerify(Copy, Results, SymCS, IC, S);
  }
}

void SymbolizeAndGeneralize(InstContext &IC,
                            Solver *S, ParsedReplacement Input) {
  std::vector<Inst *> LHSConsts, RHSConsts;
  auto Pred = [](Inst *I) {return I->K == Inst::Const;};
  findInsts(Input.Mapping.LHS, LHSConsts, Pred);
  findInsts(Input.Mapping.RHS, RHSConsts, Pred);

  // if (RHSConsts.empty()) {
  //   return;
  //   // TODO: Possible to just generalize LHS consts with preconditions?
  // }

  CandidateMap Results;

// // One at a time
// for (auto LHSConst : LHSConsts) {
//   SymbolizeAndGeneralizeImpl(IC, S, Input, {LHSConst}, RHSConsts, Results);
// }

  // All subsets?
  // TODO: Is it possible to encode this logically.

  // All at once
  SymbolizeAndGeneralizeImpl(IC, S, Input, LHSConsts, RHSConsts, Results);

  llvm::outs() << ";Input:\n";
  Input.print(llvm::outs(), true);
  llvm::outs() << "\n;Results:\n";

  // TODO : Improve sorting.
  // Here are some ideas
  // Prioritize results with more symbolic constants
  // If same number of symbolic constants - prefer lower number of runtime instructions

  auto cmp = [](const std::string &a, const std::string &b) {
    if (a.length() < b.length()) {
      return true;
    } else if (a.length() == b.length()) {
      return a < b;
    } else {
      return false;
    }
  };
  std::set<std::string, decltype(cmp)> ResultStrs(cmp);
  for (auto &&Result : Results) {
    std::string str;
    llvm::raw_string_ostream ostr(str);
    Result.print(ostr, true);
    ResultStrs.insert(str);
  }

  int n = 5;
  for (auto &&Str : ResultStrs) {
//    if (!n--) break;
    llvm::outs() << Str << "\n";
  }
}

size_t InferWidth(Inst::Kind K, const std::vector<Inst *> &Ops) {
  switch (K) {
    case Inst::LShr:
    case Inst::Shl:
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

Inst *CloneInst(InstContext &IC, Inst *I, std::map<Inst *, Inst *> &Vars) {
  if (I->K == Inst::Var) {
    return Vars[I];
  } else if (I->K == Inst::Const) {
    // llvm_unreachable("Const");
    auto Goal = Vars.begin()->second->Width; // TODO Infer.
    auto NewVal = I->Val.isSignBitSet() ? I->Val.sextOrTrunc(Goal) : I->Val.zextOrTrunc(Goal);
    return IC.getConst(NewVal);
  } else {
    std::vector<Inst *> Ops;
    for (auto Op : I->Ops) {
      Ops.push_back(CloneInst(IC, Op, Vars));
    }
    return IC.getInst(I->K, InferWidth(I->K, Ops), Ops);
  }
}

void GeneralizeBitWidth(InstContext &IC, Solver *S,
                     ParsedReplacement Input) {
  auto Vars = IC.getVariablesFor(Input.Mapping.LHS);

  assert(Vars.size() == 1 && "Multiple variables unimplemented.");

  for (int i = 1; i <= 64; ++i) {
    std::map<Inst *, Inst *> Reps;
    Reps[Vars[0]] = IC.createVar(i, Vars[0]->Name);

    auto LHS = CloneInst(IC, Input.Mapping.LHS, Reps);
    auto RHS = CloneInst(IC, Input.Mapping.RHS, Reps);

    if (!GeneralizeWidthVerify && !MinimizeWidth) {
      ReplacementContext RC;
      auto str = RC.printInst(LHS, llvm::outs(), true);
      llvm::outs() << "infer " << str << "\n";
      str = RC.printInst(RHS, llvm::outs(), true);
      llvm::outs() << "result " << str << "\n\n";
    } else {
      bool Valid;
      InstMapping M(LHS, RHS);
      std::vector<std::pair<Inst *, APInt>> Models;
      if (std::error_code EC = S->isValid(IC, {}, {}, M, Valid, &Models)) {
        llvm::errs() << EC.message() << '\n';
      }

      if (Valid) {
        if (MinimizeWidth) {
          ReplacementContext RC;
          auto str = RC.printInst(LHS, llvm::outs(), true);
          llvm::outs() << "infer " << str << "\n";
          str = RC.printInst(RHS, llvm::outs(), true);
          llvm::outs() << "result " << str << "\n\n";
          break;
        }
        llvm::outs () << "valid " << i << "\n";
      } else {
        if (!MinimizeWidth) {
          llvm::outs () << "invalid " << i << "\n";
        }
      }

    }
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

std::vector<std::string> ReduceAndGeneralize(InstContext &IC,
                               Solver *S, ParsedReplacement Input) {
  std::vector<std::pair<Inst *, APInt>> Models;
  bool Valid;
  if (std::error_code EC = S->isValid(IC, Input.BPCs, Input.PCs, Input.Mapping, Valid, &Models)) {
    llvm::errs() << EC.message() << '\n';
  }
  if (!Valid) {
    llvm::errs() << "Invalid Input.\n";
    return {};
  }

  Reducer R(IC, S);

  std::vector<ParsedReplacement> Results;
  Input = R.ReducePCs(Input);
  Input = R.ReduceRedundantPhis(Input);
  Input = R.ReduceGreedy(Input);
  Input = R.WeakenKB(Input);
  Input = R.WeakenCR(Input);
  Input = R.WeakenDB(Input);
  if (ReduceKBIFY) {
    Input = R.ReduceGreedyKBIFY(Input);
  }
  Input = R.ReducePCs(Input);
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

    if (!Everything) {
      for (auto &&S : SortedResults) {
        if (DebugLevel > 2) {
          llvm::outs() << "\n\nResult:\n";
        }
        llvm::outs() << S << '\n';
        if (!ReducePrintAll) {
          break;
        }
      }
      if (DebugLevel > 2) {
        llvm::outs() << "Number of Results: " <<SortedResults.size() << ".\n";
      }
    }
    return SortedResults;
  } else {
    if (!Everything) {
      Input.print(llvm::outs(), true);
      if (DebugLevel > 2) {
        llvm::errs() << "Failed to Generalize.\n";
      }
    }
    return {};
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
    if (Everything) {
      auto Reduced = ReduceAndGeneralize(IC, S.get(), Input);

      ParsedReplacement Rep;

      if (Reduced.empty()) {
        Rep = Input;
      } else {
        auto MB = llvm::MemoryBuffer::getMemBuffer(Reduced[0]);
        Rep =  ParseReplacements(IC, MB->getMemBufferRef().getBufferIdentifier(),
                                  Data.getBuffer(), ErrStr)[0];
      }

      Rep.print(llvm::outs(), true);

    }
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

    if (SymbolizeWidth) {
      CandidateMap Results;
      SymbolizeWidthNew(IC, S.get(), Input, Results);

      for (auto Result : Results) {
        Result.print(llvm::outs());
        llvm::outs() << "\n";
      }
    }
  }

  return 0;
}
