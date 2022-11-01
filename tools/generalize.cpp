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
    llvm::cl::init(true));

static llvm::cl::opt<bool> FindConstantRelations("relational",
    llvm::cl::desc("Find constant relations."
                   "(default=false)"),
    llvm::cl::init(true));

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
    llvm::cl::init(false));

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
    cl::init(1));

static cl::opt<bool> Everything("everything",
    cl::desc("Run everything, output one result."),
    cl::init(false));

static cl::opt<bool> JustReduce("just-reduce",
    cl::desc("JustReduce"),
    cl::init(false));

static cl::opt<bool> Basic("basic",
    cl::desc("Run all fast techniques, no synthesis^2."),
    cl::init(false));

static cl::opt<bool> Advanced("advanced",
    cl::desc("Just run more advanced stuff. Assume -basic."),
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

std::vector<Inst *> InferPotentialRelations(
        const std::vector<std::pair<Inst *, llvm::APInt>> &CMap,
        InstContext &IC, const ParsedReplacement &Input) {
  std::vector<Inst *> Results;
  if (!FindConstantRelations) {
    return Results;
  }
  // Generate a set of pairwise relations
  for (auto &&[XI, XC] : CMap) {
    for (auto &&[YI, YC] : CMap) {
      if (XI == YI || XC.getBitWidth() != YC.getBitWidth()) {
        continue;
      }
      // Add C
//      auto Diff = XC - YC;
//      Results.push_back(Builder(XI, IC).Sub(Diff).Eq(YI)());
      // Mul C
      if (YC!= 0 && XC.urem(YC) == 0) {
        auto Fact = XC.udiv(YC);
        Results.push_back(Builder(YI, IC).Mul(Fact).Eq(XI)());
      }
      if (XC != 0 && YC.urem(XC) == 0) {
        auto Fact = YC.udiv(XC);
        Results.push_back(Builder(XI, IC).Mul(Fact).Eq(YI)());
      }

      // TODO CHeck if this is too slow
      if (Input.Mapping.LHS->Width == 1) {
        // need both signed and unsigned?
        // What about s/t/e/ versions?
        if (XC.slt(YC)) Results.push_back(Builder(XI, IC).Slt(YI)());
        if (XC.ult(YC)) Results.push_back(Builder(XI, IC).Ult(YI)());
        if (YC.slt(XC)) Results.push_back(Builder(YI, IC).Slt(XI)());
        if (YC.ult(XC)) Results.push_back(Builder(YI, IC).Ult(XI)());
      }
    }
  }

  return Results;
};

// FIXME Other interesting things to try
// Symbolic KB in preconditions
// Symbolic KB with extra constraints.

// TODO Document options
void SymbolizeAndGeneralizeImpl(InstContext &IC, Solver *S, ParsedReplacement Input,
                            std::vector<Inst *> LHSConsts,
                            std::vector<Inst *> RHSConsts,
                            CandidateMap &Results) {
  
  if (RHSConsts.empty()) {
    // Handled in another function
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

  auto Relations = InferPotentialRelations(SymCS, IC, Input);
  std::vector<Inst *> Components;
  
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
  if (SymbolizeConstant || Everything) {
    for (auto &&C : SymCS) {
      if (Everything) {
        Components.push_back(Builder(C.first, IC).LogB()());
      }
      // Minus One
//      Components.push_back(Builder(C.first, IC).Sub(1)());
      // Flip bits
//      Components.push_back(Builder(C.first, IC).Xor(-1)());

      // Shiftmask
//      Components.push_back(Builder(IC, llvm::APInt::getAllOnesValue(C.first->Width)).Shl(C.first)());
      
      // Custom test
      // auto M1 = IC.getConst(llvm::APInt(C->Width, -1));
      // auto Test = Builder(C, IC).Add(M1).Xor(M1).And(M1);
      // Components.push_back(Test());
//      auto M0 = IC.getConst(llvm::APInt(C->Width, 0));
//      auto Test = Builder(M0, IC).Sub(C);
//      Components.push_back(Test());
    }
  }

  std::set<Inst *> ConcreteConsts; // for deduplication

  if (!SymbolizeConstSynthesis) {
    for (auto C : LHSConsts) {
//      ConcreteConsts.insert(C);
//      ConcreteConsts.insert(IC.getConst(llvm::APInt(C->Width, 1)));
      ConcreteConsts.insert(IC.getConst(llvm::APInt::getAllOnesValue(C->Width)));
    }
//    for (auto C : RHSConsts) {
//      ConcreteConsts.insert(C);
//    }
//    for (auto C : ConcreteConsts) {
//      Components.push_back(C);
//    }
  }

  for (auto &&ConC : ConcreteConsts) {
    Components.push_back(ConC);
  }

  for (auto &&SymC : SymCS) {
    Components.push_back(SymC.first);
  }

  std::vector<std::vector<Inst *>> Candidates;

  ConcreteInterpreter CI(VC);
  for (auto &&Target : RHSConsts) {
    Candidates.push_back({});
    EnumerativeSynthesis ES;
    auto Guesses = ES.generateExprs(IC, SymbolizeNumInsts, Components,
                                    Target->Width);
    for (auto &&Guess : Guesses) {

      if (ConcreteConsts.find(Guess) != ConcreteConsts.end()) {
        continue;
      }

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

//  SynthesisContext SC{IC, S->getSMTLIBSolver(), Input.Mapping.LHS, nullptr, Input.PCs, Input.BPCs, false, 10};
  auto LHS = Replace(Input.Mapping.LHS, IC, InstCache);
//  std::vector<Inst *> NewVars;
//  findVars(LHS, NewVars);

//  PruningManager Pr(SC, Vars, 0);

  // Generate all combination of candidates
  std::vector<std::vector<int>> Combinations = GetCombinations(Counts);

  for (auto &&Comb : Combinations) {
    static int SymExprCount = 0;
    auto InstCacheRHS = InstCache;
    for (int i = 0; i < RHSConsts.size(); ++i) {
      InstCacheRHS[RHSConsts[i]] = Candidates[i][Comb[i]];
      if (Candidates[i][Comb[i]]->K != Inst::Var) {
        Candidates[i][Comb[i]]->Name = std::string("constexpr_") + std::to_string(SymExprCount++);
      }
    }

    auto RHS = Replace(Input.Mapping.RHS, IC, InstCacheRHS);

    InstMapping Mapping(LHS, RHS);
    auto Copy = Input;
    Copy.Mapping = Mapping;
    
    //if (Pr.isInfeasible(RHS, 0)) {
//      llvm::errs() << "PRUNED\n"; // Figure out why this never succeeds :(
    //} else {
      //llvm::errs() << "NP";
    if (!InferPreconditionsAndVerify(Copy, Results, SymCS, IC, S)) {
      for (auto R : Relations) {
        Copy.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});
        InferPreconditionsAndVerify(Copy, Results, SymCS, IC, S);
        Copy.PCs.pop_back();
      }
    }

    //}
  }
}

// This is a simpler version of the above
void SymbolizeAndGeneralizeOnlyLHS(InstContext &IC,
                                   Solver *S,
                                   ParsedReplacement Input,
                                   const std::vector<Inst *> &LHSConsts,
                                   CandidateMap &Results) {
  if (LHSConsts.empty()) {
    return; // What are we even doing here?
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
  
  auto LHS = Replace(Input.Mapping.LHS, IC, InstCache);
  auto RHS = Replace(Input.Mapping.RHS, IC, InstCache);
  
  InstMapping Mapping(LHS, RHS);
  auto Copy = Input;
  Copy.Mapping = Mapping;

  if (!InferPreconditionsAndVerify(Copy, Results, SymCS, IC, S)) {
    auto Relations = InferPotentialRelations(SymCS, IC, Input);
    for (auto R : Relations) {
      Copy.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});
      InferPreconditionsAndVerify(Copy, Results, SymCS, IC, S);
      Copy.PCs.pop_back();
    }
  }
}

void SymbolizeAndGeneralize(InstContext &IC,
                            Solver *S, ParsedReplacement Input) {
  std::vector<Inst *> LHSConsts, RHSConsts;
  auto Pred = [](Inst *I) {return I->K == Inst::Const;};
  findInsts(Input.Mapping.LHS, LHSConsts, Pred);
  findInsts(Input.Mapping.RHS, RHSConsts, Pred);

  CandidateMap Results;
  
  if (RHSConsts.empty()) {
    SymbolizeAndGeneralizeOnlyLHS(IC, S, Input, LHSConsts, Results);
  } else {
   // One at a time
   for (auto LHSConst : LHSConsts) {
     SymbolizeAndGeneralizeImpl(IC, S, Input, {LHSConst}, RHSConsts, Results);
   }

    // All subsets?
    // TODO: Is it possible to encode this logically.

    // All at once
    SymbolizeAndGeneralizeImpl(IC, S, Input, LHSConsts, RHSConsts, Results);
  }
  if (!Everything) {
    llvm::outs() << ";Input:\n";
    Input.print(llvm::outs(), true);
    llvm::outs() << "\n;Results:\n";
  }
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
    if (Everything) {
        break;
    }
  }
  if (ResultStrs.empty()) {
    Input.print(llvm::outs(), true);
  }
}

std::set<Inst *> findConcreteConsts(Inst *I) {
  std::vector<Inst *> Results;
  std::set<Inst *> Ret;
  auto Pred = [](Inst *I) {return I->K == Inst::Const;};
  findInsts(I, Results, Pred);
  for (auto R : Results) {
    Ret.insert(R);
  }
  return Ret;
}

ParsedReplacement DFPreconditionsAndVerifyGreedy(
  ParsedReplacement Input, InstContext &IC, Solver *S,
  std::map<Inst *, llvm::APInt> SymCS) {

  std::map<Inst *, std::pair<llvm::APInt, llvm::APInt>> Restore;

  size_t BitsWeakened = 0;

  auto Clone = souper::Clone(Input, IC);

  for (auto &&C : SymCS) {
    if (C.first->Width < 8) continue;
    Restore[C.first] = {C.first->KnownZeros, C.first->KnownOnes};
    C.first->KnownZeros = ~C.second;
    C.first->KnownOnes = C.second;
  }

  ParsedReplacement Ret;
  auto SOLVE = [&]() -> bool {
    Ret = Verify(Input, IC, S);
    if (Ret.Mapping.LHS && Ret.Mapping.RHS) {
      return true;
    } else {
      return false;
    }
  };

  for (auto &&C : SymCS) {
    if (C.first->Width < 8) continue;
    for (size_t i = 0; i < C.first->Width; ++i) {
      llvm::APInt OriZ = C.first->KnownZeros;
      llvm::APInt OriO = C.first->KnownOnes;

      if (OriO[i] == 0 && OriZ[i] == 0) {
        continue;
      }

      if (OriO[i] == 1) C.first->KnownOnes.clearBit(i);
      if (OriZ[i] == 1) C.first->KnownZeros.clearBit(i);

      if (!SOLVE()) {
        C.first->KnownZeros = OriZ;
        C.first->KnownOnes = OriO;
      } else {
        BitsWeakened++;
      }
    }
  }

  if (BitsWeakened >= 32) { // compute better threshold somehow
    return Input;
  } else {
    for (auto &&P : Restore) {
      P.first->KnownZeros = P.second.first;
      P.first->KnownOnes = P.second.second;
    }
    Clone.Mapping.LHS = nullptr;
    Clone.Mapping.RHS = nullptr;
    return Clone;
  }

}

ParsedReplacement SimplePreconditionsAndVerifyGreedy(
        ParsedReplacement Input, InstContext &IC,
        Solver *S, std::map<Inst *, llvm::APInt> SymCS) {
  // Assume Input is not valid
  std::map<Inst *, llvm::APInt> NonBools;
  for (auto &&C : SymCS) {
    if (C.first->Width != 1) {
      NonBools.insert(C);
    }
  }
  std::swap(SymCS, NonBools);

  ParsedReplacement Clone;
  Clone.Mapping.LHS = nullptr;
  Clone.Mapping.RHS = nullptr;

  auto SOLVE = [&]() -> bool {
    Clone = Verify(Input, IC, S);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return true;
    } else {
      return false;
    }
  };

  std::vector<Inst *> Insts;
  findVars(Input.Mapping.LHS, Insts);

  std::vector<std::map<Inst *, llvm::APInt>> Inputs;
  Inputs.push_back({});
  for (auto &&P : SymCS) {
    Inputs.back()[P.first] = P.second;
  }

  std::map<Inst *, std::vector<llvm::APInt>> CVals;

  for (auto &&I : Inputs) {
    for (auto &&P: I) {
      CVals[P.first].push_back(P.second);
    }
  }

#define DF(Fact, Check)                                    \
if (All(CVals[C], [](auto Val) { return Check;})) {        \
C->Fact = true; auto s = SOLVE(); C->Fact = false;         \
if(s) return Clone;};

   for (auto &&P : SymCS) {
     auto C = P.first;
     DF(PowOfTwo, Val.isPowerOf2());
     DF(NonNegative, Val.uge(0));
     DF(NonZero, Val != 0);
     DF(Negative, Val.slt(0));
   }
#undef DF

  return Clone;
}

ParsedReplacement
FirstValidCombination(ParsedReplacement Input,
                      const std::vector<Inst *> &Targets,
                      const std::vector<std::vector<Inst *>> &Candidates,
                      std::map<Inst *, Inst *> InstCache,
                      InstContext &IC, Solver *S,
                      std::map<Inst *, llvm::APInt> SymCS,
                      bool GEN,
                      bool SDF,
                      bool DFF) {
  std::vector<int> Counts;
  for (auto &&Cand : Candidates) {
    Counts.push_back(Cand.size());
  }

  auto Combinations = GetCombinations(Counts);

  size_t IterLimit = 2000;
  size_t CurIter = 0;

  for (auto &&Comb : Combinations) {
    if (CurIter >= IterLimit) {
      break;
    } else {
      CurIter++;
    }

    static int SymExprCount = 0;
    auto InstCacheRHS = InstCache;

    std::vector<Inst *> VarsFound;

    for (int i = 0; i < Targets.size(); ++i) {
      InstCacheRHS[Targets[i]] = Candidates[i][Comb[i]];
      findVars(Candidates[i][Comb[i]], VarsFound);
      if (Candidates[i][Comb[i]]->K != Inst::Var) {
        Candidates[i][Comb[i]]->Name = std::string("constexpr_") + std::to_string(SymExprCount++);
      }
    }

    std::set<Inst *> SymsInCurrent;
    for (auto &&V : VarsFound) {
      if (V->Name.starts_with("sym")) {
        SymsInCurrent.insert(V);
      }
    }

    std::map<Inst *, Inst *> ReverseMap;

    for (auto &&[C, Val] : SymCS) {
      if (SymsInCurrent.find(C) == SymsInCurrent.end()) {
        ReverseMap[C] = Builder(IC, Val)();
      }
    }

    auto Clone = Input;
    Clone.Mapping.LHS = nullptr;
    Clone.Mapping.RHS = nullptr;

    auto SOLVE = [&](ParsedReplacement P) -> bool {
//      P.print(llvm::errs(), true);
//      llvm::errs() << "\n";

      if (GEN) {
        Clone = Verify(P, IC, S);

        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return true;
        }
      }

      if (DFF) {
        Clone = DFPreconditionsAndVerifyGreedy(P, IC, S, SymCS);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return true;
        }
      }

      if (SDF) {
        Clone = SimplePreconditionsAndVerifyGreedy(P, IC, S, SymCS);

        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return true;
        }
      }

      return false;
    };

    auto Copy = Replace(Input, IC, InstCacheRHS);
    if (SOLVE(Copy)) {
      return Clone;
    }

    Copy = Replace(Copy, IC, ReverseMap);
    if (SOLVE(Copy)) {
      return Clone;
    }

  }

  Input.Mapping.LHS = nullptr;
  Input.Mapping.RHS = nullptr;
  return Input;
}


std::vector<Inst *> IOSynthesize(llvm::APInt Target, std::set<Inst *> LHSConsts,
                  std::map<Inst *, Inst *> SMap, InstContext &IC, size_t Threshold, bool ConstMode) {

  std::vector<Inst *> Results;

  // Just symbolic or Concrete constant

  for (auto C : LHSConsts) {
    if (!ConstMode) {
      if (C->Width == Target.getBitWidth() && C->Val == Target) {
        Results.push_back(SMap[C]);
      }
    } else {
      if (C->Width == Target.getBitWidth()) {
        Results.push_back(Builder(IC, Target)());
      }
    }
  }

  if (!Threshold) {
    return Results;
  }

  // Recursive formulation

  for (auto C : LHSConsts) {
    if (C->Width != Target.getBitWidth()) {
      continue;
    }

    // Binary operators

    // C + X == Target
    for (auto X : IOSynthesize(Target - C->Val, LHSConsts, SMap, IC,
                               Threshold - 1, ConstMode)) {
      Results.push_back(Builder(SMap[C], IC).Add(X)());
    }

    // C - X == Target
    for (auto X : IOSynthesize(C->Val - Target, LHSConsts, SMap, IC,
                               Threshold - 1, ConstMode)) {
      Results.push_back(Builder(SMap[C], IC).Sub(X)());
    }

    // X - C == Target
    for (auto X : IOSynthesize(Target + C->Val, LHSConsts, SMap, IC,
                               Threshold - 1, ConstMode)) {
      Results.push_back(Builder(X, IC).Sub(SMap[C])());
    }

    // C * X == Target
    if (C->Val.isNegative() || Target.isNegative()) {
      if (C->Val != 0 && Target.srem(C->Val) == 0) {
        for (auto X : IOSynthesize(Target.sdiv(C->Val), LHSConsts, SMap,
                                   IC, Threshold - 1, ConstMode)) {
          Results.push_back(Builder(X, IC).Mul(SMap[C])());
        }
      }
    } else {
      if (C->Val != 0 && Target.urem(C->Val) == 0) {
        for (auto X : IOSynthesize(Target.udiv(C->Val), LHSConsts, SMap,
                                   IC, Threshold - 1, ConstMode)) {
          Results.push_back(Builder(X, IC).Mul(SMap[C])());
        }
      }
    }

    // C / X == Target
    if (C->Val.isNegative() || Target.isNegative()) {
      if (Target != 0 && C->Val.srem(Target) == 0) {
        for (auto X : IOSynthesize(C->Val.sdiv(Target), LHSConsts, SMap,
                                   IC, Threshold - 1, ConstMode)) {
          Results.push_back(Builder(SMap[C], IC).SDiv(X)());
        }
      }
    } else {
      if (Target != 0 && C->Val.urem(Target) == 0) {
        for (auto X : IOSynthesize(C->Val.udiv(Target), LHSConsts, SMap,
                                   IC, Threshold - 1, ConstMode)) {
          Results.push_back(Builder(SMap[C], IC).UDiv(X)());
        }
      }
    }

    // X / C == Target

    if (C->Val.isNegative() || Target.isNegative()) {
      if (C->Val != 0 && Target.srem(C->Val) == 0) {
        for (auto X : IOSynthesize(C->Val * Target, LHSConsts, SMap,
                                   IC, Threshold - 1, ConstMode)) {
          Results.push_back(Builder(X, IC).SDiv(SMap[C])());
        }
      }
    } else {
      if (C->Val != 0 && Target.urem(C->Val) == 0) {
        for (auto X : IOSynthesize(C->Val * Target, LHSConsts, SMap,
                                   IC, Threshold - 1, ConstMode)) {
          Results.push_back(Builder(X, IC).UDiv(SMap[C])());
        }
      }
    }

    // Shifts?

    // Unary operators (no recursion required)
    if (Target == C->Val.logBase2()) {
      Results.push_back(Builder(SMap[C], IC).LogB()());
    }
    if (Target == C->Val.reverseBits()) {
      Results.push_back(Builder(SMap[C], IC).BitReverse()());
    }
    // TODO Add others

    if (Threshold == 1) {
      for (auto C2 : LHSConsts) {
        if (C == C2 || C->Width != C2->Width) {
          continue;
        }
        if ((C->Val & C2->Val) == Target) {
          Results.push_back(Builder(SMap[C], IC).And(SMap[C2])());
        }
        if ((C->Val | C2->Val) == Target) {
          Results.push_back(Builder(SMap[C], IC).Or(SMap[C2])());
        }
        if ((C->Val ^ C2->Val) == Target) {
          Results.push_back(Builder(SMap[C], IC).Xor(SMap[C2])());
        }
      }
    }
  }

  return Results;
}

std::vector<std::vector<Inst *>>
InferSpecialConstExprsAllSym(std::vector<Inst *> RHS, std::set<Inst *>
                      LHS, std::map<Inst *, Inst *> SMap,
                             InstContext &IC) {
  std::vector<std::vector<Inst *>> Results;
  for (auto R : RHS) {
    Results.push_back(IOSynthesize(R->Val, LHS, SMap, IC, 3, false));
  }
  return Results;
}

std::vector<std::vector<Inst *>>
InferSpecialConstExprsWithConcretes(std::vector<Inst *> RHS, std::set<Inst *>
                      LHS, std::map<Inst *, Inst *> SMap,
                             InstContext &IC) {
  std::vector<std::vector<Inst *>> Results;
  for (auto R : RHS) {
    auto Cands = IOSynthesize(R->Val, LHS, SMap, IC, 3, true);
    std::vector<Inst *> Filtered;
    for (auto Cand : Cands) {
      if (Cand->K != Inst::Const) {
        Filtered.push_back(Cand);
      }
    }
    Results.push_back(Filtered);
  }
  return Results;
}

std::vector<std::vector<Inst *>> Enumerate(std::vector<Inst *> RHSConsts,
                                           std::set<Inst *> LHSConsts, InstContext &IC,
                                           size_t NumInsts = 1) {
    std::vector<std::vector<Inst *>> Candidates;

    std::vector<Inst *> Components;
    for (auto &&C : LHSConsts) {
      Components.push_back(C);
      Components.push_back(Builder(C, IC).Sub(1)());
      Components.push_back(Builder(C, IC).Xor(-1)());
      Components.push_back(Builder(IC, llvm::APInt::getAllOnesValue(C->Width)).Shl(C)());
    }

    // TODO: Custom components

    for (auto &&Target : RHSConsts) {
      Candidates.push_back({});
      EnumerativeSynthesis ES;
      auto Guesses = ES.generateExprs(IC, NumInsts, Components,
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

    return Candidates;
}

void findDangerousConstants(Inst *I, std::set<Inst *> &Results) {
  std::set<Inst *> Visited;
  std::vector<Inst *> Stack{I};
  while (!Stack.empty()) {
    auto Cur = Stack.back();
    Stack.pop_back();
    Visited.insert(Cur);
    if (Visited.find(Cur) == Visited.end()) {
      continue;
    }
    for (auto Child : Cur->Ops) {
      if (Cur->K == Inst::ExtractValue) {
        if (Child->K == Inst::Const) {
          // Constant operands of ExtractValue instructions
          Results.insert(Child);
        }
      }
      Stack.push_back(Child);
    }
  }
}

// Assuming the input has leaves pruned and preconditions weakened
ParsedReplacement SuccessiveSymbolize(InstContext &IC,
                            Solver *S, ParsedReplacement Input, bool &Changed) {

  // Print first successful result and exit, no result sorting.

  // Prelude

  auto Fresh = Clone(Input, IC);
  auto Refresh = [&] (auto Msg) {
    Input = Fresh;
//    llvm::errs() << "POST " << Msg << "\n";
    Changed = true;
    return Fresh;
  };

  auto LHSConsts = findConcreteConsts(Input.Mapping.LHS);
  auto RHSConsts = findConcreteConsts(Input.Mapping.RHS);

  std::set<Inst *> ConstsBlackList;
  findDangerousConstants(Input.Mapping.LHS, ConstsBlackList);
  findDangerousConstants(Input.Mapping.RHS, ConstsBlackList);

  for (auto &&C : ConstsBlackList) {
    LHSConsts.erase(C);
    RHSConsts.erase(C);
  }


  ParsedReplacement Result = Input;

  std::map<Inst *, Inst *> SymConstMap;

  std::map<Inst *, Inst *> InstCache;

  std::map<Inst *, llvm::APInt> SymCS;

  int i = 1;
  for (auto I : LHSConsts) {
    SymConstMap[I] = IC.createVar(I->Width, "symconst_" +
                                  std::to_string(i++));

    InstCache[I] = SymConstMap[I];
    SymCS[SymConstMap[I]] = I->Val;
  }

  for (auto I : RHSConsts) {
    if (SymConstMap.find(I) != SymConstMap.end()) {
      continue;
    }
    SymConstMap[I] = IC.createVar(I->Width, "symconst_" +
                                  std::to_string(i++));
    InstCache[I] = SymConstMap[I];
//    SymCS[SymConstMap[I]] = I->Val;
  }

  std::vector<Inst *> RHSFresh; // RHSConsts - LHSConsts

  for (auto C : RHSConsts) {
    if (LHSConsts.find(C) == LHSConsts.end()) {
      RHSFresh.push_back(C);
    }
  }

  Refresh("Prelude");
  // Step 1 : Just direct symbolize for common consts, no constraints

  std::map<Inst *, Inst *> CommonConsts;
  for (auto C : RHSConsts) {
    if (LHSConsts.find(C) != LHSConsts.end()) {
      CommonConsts[C] = SymConstMap[C];
    }
  }

  if (!CommonConsts.empty()) {
    Result = Replace(Result, IC, CommonConsts);

    auto Clone = Verify(Result, IC, S);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    Clone = SimplePreconditionsAndVerifyGreedy(Result, IC, S, SymCS);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    Clone = DFPreconditionsAndVerifyGreedy(Result, IC, S, SymCS);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

  }

  Refresh("Direct Symbolize for common consts");

  // Step 1.5 : Direct symbolize, simple rel constraints on LHS

  std::vector<std::pair<Inst *, llvm::APInt>> CMap;

  for (auto &&C : LHSConsts) {
    CMap.push_back({SymConstMap[C], C->Val});
  }

  auto Relations = InferPotentialRelations(CMap, IC, Input);

  std::map<Inst *, Inst *> JustLHSSymConstMap;

  for (auto &&C : LHSConsts) {
    JustLHSSymConstMap[C] = SymConstMap[C];
  }

  auto Copy = Replace(Input, IC, JustLHSSymConstMap);
  for (auto &&R : Relations) {
    Copy.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});
    auto Clone = Verify(Copy, IC, S);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Copy.PCs.pop_back();
  }

  Refresh("Direct + simple rel constraints");

  // Step 2 : Symbolize LHS Consts with KB, CR, SimpleDF constrains
  if (RHSFresh.empty()) {
    auto Copy = Replace(Input, IC, JustLHSSymConstMap);

    auto Clone = SimplePreconditionsAndVerifyGreedy(Copy, IC, S, SymCS);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    Refresh("LHS Constraints");

    Clone = DFPreconditionsAndVerifyGreedy(Copy, IC, S, SymCS);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
  }
  Refresh("All LHS Constraints");


  // Step 3 : Special RHS constant exprs, no constants

  if (!RHSFresh.empty()) {

  std::vector<std::vector<Inst *>> SimpleCandidates =
    InferSpecialConstExprsAllSym(RHSFresh, LHSConsts, SymConstMap, IC);

  if (!SimpleCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidates,
                                       InstCache, IC, S, SymCS,
                                       true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
  }

  Refresh("Special expressions, no constants");


  // Step 4 : Enumerated expressions

  std::set<Inst *> Components;
  for (auto C : LHSConsts) {
    Components.insert(SymConstMap[C]);
  }

  auto EnumeratedCandidates = Enumerate(RHSFresh, Components, IC);

  if (!EnumeratedCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                  InstCache, IC, S, SymCS, true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Enumerated cands, no constraints");

    // Enumerated Expressions with some relational constraints
    if (CMap.size() == 2) {
      for (auto &&R : Relations) {
        Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

        auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                           InstCache, IC, S, SymCS, true, false, false);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return Clone;
        }
      }
    }
    Refresh("Relational constraints for enumerated cands.");
  }


  // Step 4.75 : Enumerate 2 instructions when single RHS Constant.
  if (RHSFresh.size() == 1) {
    auto EnumeratedCandidatesTwoInsts = Enumerate(RHSFresh, Components, IC, 2);

    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidatesTwoInsts,
                                  InstCache, IC, S, SymCS, true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Enumerated 2 insts");

    // Enumerated Expressions with some relational constraints
    if (CMap.size() == 2) {
      for (auto &&R : Relations) {
        Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

        auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidatesTwoInsts,
                                           InstCache, IC, S, SymCS, true, false, false);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return Clone;
        }
      }
      Refresh("Enumerated 2 insts some constraints");
    }
  }

    // Step 4.8 : Special RHS constant exprs, with constants

    std::vector<std::vector<Inst *>> SimpleCandidatesWithConsts =
      InferSpecialConstExprsWithConcretes(RHSFresh, LHSConsts, SymConstMap, IC);

    if (!SimpleCandidatesWithConsts.empty()) {
      auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                         InstCache, IC, S, SymCS,
                                         true, false, false);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
    }

    Refresh("Special expressions, with constants");

  // Step 5 : Simple exprs with constraints

  if (!SimpleCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidates,
                                       InstCache, IC, S, SymCS, false, true, true);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Simple cands with constraints");

    for (auto &&R : Relations) {
      Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

      auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidates,
                                         InstCache, IC, S, SymCS, true, true, true);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }

      Input.PCs.pop_back();
    }
    Refresh("Simple cands with constraints and relations");
  }

  // Step 5.5 : Simple exprs with constraints

  if (!SimpleCandidatesWithConsts.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                       InstCache, IC, S, SymCS, false, true, true);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Simple cands+consts with constraints");

    for (auto &&R : Relations) {
      Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

      auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                         InstCache, IC, S, SymCS, true, true, true);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }

      Input.PCs.pop_back();
    }
    Refresh("Simple cands+consts with constraints and relations");
  }

  // Step 6 : Enumerated exprs with constraints

  if (!EnumeratedCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                  InstCache, IC, S, SymCS, true, true, true);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    Refresh("Enumerated exprs with constraints");

    for (auto &&R : Relations) {
      Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

      auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                         InstCache, IC, S, SymCS, true, true, true);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
    }
    Refresh("Enumerated exprs with constraints and relations");
  }

//  std::vector<Inst *> SymDFVars;
//  std::set<Inst *> LHSConstsAndSymDF;

//  auto Clone = souper::Clone(Input, IC);
//  std::vector<Inst *> Vars;
//  findVars(Clone.Mapping.LHS, Vars);


//  std::set<Inst *> Visited;
//  for (size_t i = 0; i < Vars.size(); ++i) {
//    if (Visited.find(Vars[i]) != Visited.end()/* || Vars[i]->Width == 7*/) {
//      continue;
//    }
//    if (Vars[i]->KnownOnes.getBitWidth() != Vars[i]->Width ||
//        Vars[i]->KnownZeros.getBitWidth() != Vars[i]->Width) {
//      continue;
//    }

//    SymDFVars.push_back(IC.createVar(Vars[i]->Width, "skb_one_" + std::to_string(i)));
//    SymDFVars.back()->SymOneOf = Vars[i];
//    Vars[i]->SymKnownOnes = SymDFVars.back();
//    LHSConstsAndSymDF.insert(SymDFVars.back());
//    Vars[i]->KnownOnes = llvm::APInt();

//    SymDFVars.push_back(IC.createVar(Vars[i]->Width, "skb_zero_" + std::to_string(i)));
//    SymDFVars.back()->SymZeroOf = Vars[i];
//    Vars[i]->SymKnownZeros = SymDFVars.back();
//    LHSConstsAndSymDF.insert(SymDFVars.back());
//    Vars[i]->KnownZeros = llvm::APInt();
//  }


//  Refresh("Symbolic known bits");
  }
  Refresh("END");
  Changed = false;
  return Input;
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

ParsedReplacement ReduceBasic(InstContext &IC,
                              Solver *S, ParsedReplacement Input) {
  Reducer R(IC, S);
  Input = R.ReducePCs(Input);
  Input = R.ReduceRedundantPhis(Input);
  Input = R.ReduceGreedy(Input);
  Input = R.ReducePairsGreedy(Input);
  Input = R.ReduceTriplesGreedy(Input);
  Input = R.WeakenKB(Input);
  Input = R.WeakenCR(Input);
  Input = R.WeakenDB(Input);
  Input = R.WeakenOther(Input);
  if (ReduceKBIFY) {
    Input = R.ReduceGreedyKBIFY(Input);
  }
  Input = R.ReducePCs(Input);
  Input = R.ReducePCsToDF(Input);
  return Input;
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
  Input = R.WeakenOther(Input);
  if (ReduceKBIFY) {
    Input = R.ReduceGreedyKBIFY(Input);
  }
  Input = R.ReducePCs(Input);

//  llvm::outs() << "HERE5\n";
  Input.print(llvm::outs(), true);
  return {Input.getString(true)};

  R.ReduceRec(Input, Results);

  if (DebugLevel > 3) {
    R.Stats();
  }
//  llvm::errs() << "HERE0: " << Results.size();
  if (!Results.empty()) {
    std::set<std::string> DedupedResults;
    for (auto &&Result : Results) {
      DedupedResults.insert(Result.getString(false));
    }

    std::vector<std::string> SortedResults(DedupedResults.begin(), DedupedResults.end());
    std::sort(SortedResults.begin(), SortedResults.end(), [](auto a, auto b){return a.length() < b.length();});

    if (Basic) {
//      llvm::errs() << "HERE: " << SortedResults.size();
      return SortedResults;
    }

    if (!Everything && !Basic && !Advanced) {
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
    if (!Everything && !Basic && !Advanced) {
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
    if (Basic) {
      ParsedReplacement Result = ReduceBasic(IC, S.get(), Input);
      if (!JustReduce) {
        bool Changed = false;
        size_t MaxTries = 1; // Increase this if we even run with 10/100x timeout.
        do {
          Result = ReduceBasic(IC, S.get(), Input);
          Result = SuccessiveSymbolize(IC, S.get(), Result, Changed);
//          Result.print(llvm::errs(), true);
        } while (--MaxTries && Changed);
      }
      Result.print(llvm::outs(), true);
      llvm::outs() << "\n";
      continue;
    }

//    if (Advanced) {
//      auto Result = SuccessiveSymbolize(IC, S.get(), Input);
//      Result.print(llvm::outs());
//      llvm::outs() << "\n";
//      continue;
//    }

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

// TODO List
// Derive relations between LHSConsts and use them as preconditions
// Test phase before verification
// Is it possible to pre-generate the set of all possible constants? No.
// Some? definitely.
