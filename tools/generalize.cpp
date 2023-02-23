#define _LIBCPP_DISABLE_DEPRECATION_WARNINGS

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"

#include "souper/Infer/AliveDriver.h"
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

static llvm::cl::opt<bool> ReduceKBIFY("reduce-kbify",
    llvm::cl::desc("Try to reduce the number of instructions by introducing known bits constraints."
                   "(default=false)"),
    llvm::cl::init(true));

static llvm::cl::opt<bool> FindConstantRelations("relational",
    llvm::cl::desc("Find constant relations."
                   "(default=true)"),
    llvm::cl::init(true));

static llvm::cl::opt<size_t> SymbolizeNumInsts("symbolize-num-insts",
    llvm::cl::desc("Number of instructions to synthesize"
                   "(default=1)"),
    llvm::cl::init(1));

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

static cl::opt<size_t> NumResults("generalization-num-results",
    cl::desc("Number of Generalization Results"),
    cl::init(1));

static cl::opt<bool> JustReduce("just-reduce",
    cl::desc("JustReduce"),
    cl::init(false));

static cl::opt<bool> Basic("basic",
    cl::desc("Run all fast techniques."),
    cl::init(false));

static cl::opt<bool> OnlyWidth("only-width",
    cl::desc("Only infer width checks, no synthesis."),
    cl::init(false));

static cl::opt<bool> NoWidth("no-width",
    cl::desc("No width independence checks."),
    cl::init(false));


static cl::opt<bool> Advanced("advanced",
    cl::desc("Just run more advanced stuff. Assume -basic."),
    cl::init(false));

static cl::opt<bool> SymbolicDF("symbolic-df",
    cl::desc("Generalize with symbolic dataflow facts."),
    cl::init(false));

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

template <typename C, typename F>
bool All(const C &c, F f) {
  for (auto &&m : c) {
    if (!f(m)) {
      return false;
    }
  }
  return true;
}

std::vector<Inst *> findConcreteConsts(const ParsedReplacement &Input) {
  std::vector<Inst *> Consts;
  auto Pred = [](Inst *I) {
    return I->K == Inst::Const && I->Name.find("sym") == std::string::npos;
  };

  findInsts(Input.Mapping.LHS, Consts, Pred);
  findInsts(Input.Mapping.RHS, Consts, Pred);
  std::set<Inst *> ResultSet; // For deduplication
  for (auto &&C : Consts) {
    ResultSet.insert(C);
  }
  std::vector<Inst *> Result;
  for (auto &&C : ResultSet) {
    Result.push_back(C);
  }
  return Result;
}

std::vector<Inst *> FilterRelationsByValue(const std::vector<Inst *> &Relations,
                        const std::vector<std::pair<Inst *, llvm::APInt>> &CMap) {
  std::unordered_map<Inst *, EvalValue> ValueCache;
  for (auto &&[I, V] : CMap) {
    ValueCache[I] = EvalValue(V);
  }
  ConcreteInterpreter CI(ValueCache);
  std::vector<Inst *> FilteredRelations;
  for (auto &&R : Relations) {
    auto Result = CI.evaluateInst(R);
    // llvm::errs() << "HERE: " << Result.getValue().toString(2, false) << "\n";
    if (Result.hasValue() && Result.getValue().isAllOnesValue()) {
      FilteredRelations.push_back(R);
    }
  }
  return FilteredRelations;
}

std::vector<Inst *> InferConstantLimits(
  const std::vector<std::pair<Inst *, llvm::APInt>> &CMap,
        InstContext &IC, const ParsedReplacement &Input) {
  std::vector<Inst *> Results;
  if (!FindConstantRelations) {
    return Results;
  }
  auto ConcreteConsts = findConcreteConsts(Input);
  std::sort(ConcreteConsts.begin(), ConcreteConsts.end(),
          [](auto A, auto B) {
            if (A->Width == B->Width) {
              return A->Val.ugt(B->Val);
            } else {
              return A->Width < B->Width;
            }
          });

  for (auto &&[XI, XC] : CMap) {
    // X < Width, X <= Width
    auto Width = Builder(XI, IC).BitWidth();
    Results.push_back(Builder(XI, IC).Ult(Width)());
    Results.push_back(Builder(XI, IC).Ule(Width)());

    // X slt SMAX, x ult UMAX
    auto WM1 = Width.Sub(1);
    auto SMax = Builder(IC, llvm::APInt(XI->Width, 1)).Shl(WM1).Sub(1)();
    Results.push_back(Builder(XI, IC).Slt(SMax)());

    auto gZ = Builder(XI, IC).Ugt(0)();

    Results.push_back(Builder(XI, IC).Ult(Width).And(gZ)());
    Results.push_back(Builder(XI, IC).Ule(Width).And(gZ)());

    // 2 * X < C, 2 * X >= C
    for (auto C : ConcreteConsts) {
      if (C->Width != XI->Width) {
        continue;
      }
      auto Sum = Builder(XI, IC).Add(XI)();
      Results.push_back(Builder(Sum, IC).Ult(C->Val)());
      Results.push_back(Builder(Sum, IC).Ugt(C->Val)());
    }
  }

  for (auto &&[XI, XC] : CMap) {
    for (auto &&[YI, YC] : CMap) {
      if (XI == YI) {
        continue;
      }
      auto Sum = Builder(XI, IC).Add(YI)();
      // Sum related to width
      auto Width = Builder(Sum, IC).BitWidth();
      Results.push_back(Builder(Sum, IC).Ult(Width)());
      Results.push_back(Builder(Sum, IC).Ule(Width)());
      Results.push_back(Builder(Sum, IC).Eq(Width)());

      // Sum less than const, Sum greater= than const
      for (auto C : ConcreteConsts) {
        if (Sum->Width != C->Width) {
          continue;
        }
        Results.push_back(Builder(Sum, IC).Ult(C->Val)());
        Results.push_back(Builder(Sum, IC).Ugt(C->Val)());
      }
    }
  }
  return FilterRelationsByValue(Results, CMap);
}

std::vector<Inst *> BitFuncs(Inst *I, InstContext &IC) {
  std::vector<Inst *> Results;
  Results.push_back(Builder(I, IC).CtPop()());
  Results.push_back(Builder(I, IC).Ctlz()());
  Results.push_back(Builder(I, IC).Cttz()());

  auto Copy = Results;
  for (auto &&C : Copy) {
    Results.push_back(Builder(C, IC).BitWidth().Sub(C)());
  }

  return Results;
}

// This was originally intended to find relational constraints
// but we also use to fine some ad hoc constraints now.
// TODO: Filter relations by concrete interpretation
std::vector<Inst *> InferPotentialRelations(
        const std::vector<std::pair<Inst *, llvm::APInt>> &CMap,
        InstContext &IC, const ParsedReplacement &Input) {
  std::vector<Inst *> Results;
  if (!FindConstantRelations) {
    return Results;
  }

  for (auto &&[XI, XC] : CMap) {
    for (auto &&[YI, YC] : CMap) {
      if (XI == YI || XC.getBitWidth() == YC.getBitWidth()) {
        continue;
      }
      llvm::errs() << "HERE: " << XC.getLimitedValue() << ' ' <<  YC.getLimitedValue() << '\n';
      if (XC.getLimitedValue() == YC.getLimitedValue()) {
        if (XI->Width > YI->Width) {
          Builder(YI, IC).ZExt(XI->Width).Eq(XI)()->Print();
          Results.push_back(Builder(YI, IC).ZExt(XI->Width).Eq(XI)());
        } else {
          Results.push_back(Builder(XI, IC).ZExt(YI->Width).Eq(YI)());
        }
      }
    }
  }


  // Generate a set of pairwise relations
  for (auto &&[XI, XC] : CMap) {
    // llvm::errs() << "HERE: " << XC << "\n";
    for (auto &&[YI, YC] : CMap) {

      if (XI == YI || XC.getBitWidth() != YC.getBitWidth()) {
        continue;
      }

      // // Add C
      // auto Diff = XC - YC;
      // Results.push_back(Builder(XI, IC).Sub(Diff).Eq(YI)());

      // Mul C
      if (YC!= 0 && XC.urem(YC) == 0) {
        auto Fact = XC.udiv(YC);
        Results.push_back(Builder(YI, IC).Mul(Fact).Eq(XI)());
      }
      if (XC != 0 && YC.urem(XC) == 0) {
        auto Fact = YC.udiv(XC);
        Results.push_back(Builder(XI, IC).Mul(Fact).Eq(YI)());
      }

      // TODO Check if this is too slow
      // if (Input.Mapping.LHS->Width == 1) {
        // need both signed and unsigned?
        // What about s/t/e/ versions?
        if (XC.slt(YC)) Results.push_back(Builder(XI, IC).Slt(YI)());
        if (XC.ult(YC)) Results.push_back(Builder(XI, IC).Ult(YI)());
        if (YC.slt(XC)) Results.push_back(Builder(YI, IC).Slt(XI)());
        if (YC.ult(XC)) Results.push_back(Builder(YI, IC).Ult(XI)());
      // }

      auto XBits = BitFuncs(XI, IC);
      auto YBits = BitFuncs(YI, IC);

      for (auto &&XBit : XBits) {
        for (auto &&YBit : YBits) {
          Results.push_back(Builder(XBit, IC).Ule(YBit)());
          Results.push_back(Builder(XBit, IC).Ult(YBit)());
        }
      }

      // No example yet where this is useful
      // for (auto &&XBit : XBits) {
      //   for (auto &&YBit : YBits) {
      //     Results.push_back(Builder(XBit, IC).Ne(YBit)());
      //     Results.push_back(Builder(XBit, IC).Eq(YBit)());
      //   }
      // }

    }
    Results.push_back(Builder(XI, IC).Eq(Builder(XI, IC).BitWidth().Sub(1))());
    Results.push_back(Builder(XI, IC).Eq(Builder(XI, IC).BitWidth().UDiv(2))());
    Results.push_back(Builder(XI, IC).Eq(Builder(XI, IC).BitWidth())());
  }
  // for (auto R : InferConstantLimits(CMap, IC, Input)) {
  //   Results.push_back(R);
  // }
  return FilterRelationsByValue(Results, CMap);
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

//  llvm::errs() << "HERE " << BitsWeakened << "\n";
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
     DF(PowOfTwo, Val.isPowerOf2()); // Invoke solver only if Val is a power of 2
     DF(NonNegative, Val.uge(0));
     DF(NonZero, Val != 0);
     DF(Negative, Val.slt(0));
   }
#undef DF

  return Clone;
}

ParsedReplacement SymKBPreconditionsAndVerifyGreedy(
        ParsedReplacement Input, InstContext &IC,
    Solver *S, std::map<Inst *, llvm::APInt> SymCS) {

  std::map<Inst *, Inst *> SymKMap;
  static size_t i = 0;
  for (auto &&C : SymCS) {
    SymKMap[IC.createVar(C.first->Width, "symconst_k0_" + std::to_string(i++))] = C.first;
    SymKMap[IC.createVar(C.first->Width, "symconst_k1_" + std::to_string(i++))] = C.first;
    // Only discriminant for these two are in Name
  }

  auto KPC = [&](Inst *SymK) -> InstMapping {
    // true : Constraint for known zero
    // false : Constraint for known one

    InstMapping Result;
    Result.RHS = IC.getConst(llvm::APInt(1, 1));

    auto I = SymKMap[SymK];
    auto Width = I->Width;

    auto K0 = SymK->Name.starts_with("symconst_k0_");

    if (K0) {
      Inst *AllOnes = IC.getConst(llvm::APInt::getAllOnesValue(Width));
      Inst *NotZeros = IC.getInst(Inst::Xor, Width, {SymK, AllOnes});
      Inst *VarNotZero = IC.getInst(Inst::Or, Width, {I, NotZeros});
      Inst *ZeroBits = IC.getInst(Inst::Eq, 1, {VarNotZero, NotZeros});
      Result.LHS = ZeroBits;
    } else {
      Inst *VarAndOnes = IC.getInst(Inst::And, Width, {I, SymK});
      Inst *OneBits = IC.getInst(Inst::Eq, 1, {VarAndOnes, SymK});
      Result.LHS = OneBits;
    }

    return Result;
  };

  // Pairwise expressions, all boolean

  for (auto &&P1 : SymKMap) {
    for (auto &&P2 : SymKMap) {
      if (P1.first == P2.first || P1.first->Width != P2.first->Width) {
        continue;
      }

      auto X = P1.first;
      auto Y = P2.first;

      auto W = P1.first->Width;

      std::vector<Inst *> Exprs;

      auto TrueVal = IC.getConst(llvm::APInt(1, 1));

      // bit flip x == ~y
      Exprs.push_back(Builder(X, IC).Xor(Y).Eq(llvm::APInt::getAllOnesValue(X->Width))());

      // cttz(val) + ctlz(val) >= Width
      auto XV = X;
      if (X->Name.starts_with("symconst_k0_")) {
        XV = Builder(X, IC).Xor(llvm::APInt::getAllOnesValue(W))();
      }
      auto YV = Y;
      if (Y->Name.starts_with("symconst_k0_")) {
        YV = Builder(Y, IC).Xor(llvm::APInt::getAllOnesValue(W))();
      }
      Exprs.push_back(Builder(IC, llvm::APInt(W, W)).Ule(
                        Builder(XV, IC).Cttz().Add(
                          Builder(YV, IC).Ctlz()())())());


      std::vector<Inst *> Components{X, Y, XV, YV};
      Components.push_back(IC.getConst(llvm::APInt::getAllOnesValue(W)));

      EnumerativeSynthesis ES;
      auto Guesses = ES.generateExprs(IC, 2, Components, 1);
      for (auto &&Guess : Guesses) {
        std::set<Inst *> ConstSet;
        souper::getConstants(Guess, ConstSet);
        if (!ConstSet.empty()) {
          if (SymbolizeConstSynthesis) {
            Exprs.push_back(Guess);
          }
        } else {
          Exprs.push_back(Guess);
        }
      }


      for (auto &&E : Exprs) {

        auto Backup = Input.PCs;

        InstMapping M(E, TrueVal);

        Input.PCs.push_back(M);
        Input.PCs.push_back(KPC(X));
        Input.PCs.push_back(KPC(Y));

//        Input.print(llvm::errs(), true);
//        llvm::errs() << "\n";

        auto Clone = Verify(Input, IC, S);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return Clone;
        }

        Input.PCs = Backup;
      }
    }
  }

  Input.Mapping.LHS = nullptr;
  Input.Mapping.RHS = nullptr;
  //TODO Better failure indicator
  return Input;
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

      if (SDF) {
        Clone = SimplePreconditionsAndVerifyGreedy(P, IC, S, SymCS);

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

      return false;
    };

    auto Copy = Input;
    Copy.Mapping.LHS = Replace(Input.Mapping.LHS, IC, InstCacheRHS);
    Copy.Mapping.RHS = Replace(Input.Mapping.RHS, IC, InstCacheRHS);

    // Copy.PCs = Input.PCs;
    if (SOLVE(Copy)) {
      return Clone;
    }

    Copy.Mapping.LHS = Replace(Copy.Mapping.LHS, IC, ReverseMap);
    Copy.Mapping.RHS = Replace(Copy.Mapping.RHS, IC, ReverseMap);
    if (SOLVE(Copy)) {
      return Clone;
    }

  }

  Input.Mapping.LHS = nullptr;
  Input.Mapping.RHS = nullptr;
  return Input;
}

// TODO: Prevent NOPs from being synthesized by passing parent instruction
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

    // bit flip
    llvm::APInt D = C->Val;
    D.flipAllBits();
    if (Target == D) {
      Results.push_back(Builder(SMap[C], IC).Xor(llvm::APInt::getAllOnesValue(C->Width))());
    }

    if (Target == D + 1) {
      Results.push_back(Builder(SMap[C], IC).Xor(llvm::APInt::getAllOnesValue(C->Width)).Add(1)());
    }

    // neg
    D = C->Val;
    D.negate();
    if (Target == D) {
      Results.push_back(Builder(IC, llvm::APInt::getAllOnesValue(C->Width)).Sub(SMap[C])());
    }

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

  return Results;
}

std::vector<std::vector<Inst *>>
InferSpecialConstExprsAllSym(std::vector<Inst *> RHS, std::set<Inst *>
                             LHS, std::map<Inst *, Inst *> SMap,
                             InstContext &IC, int depth = 3) {
  std::vector<std::vector<Inst *>> Results;
  for (auto R : RHS) {
    Results.push_back(IOSynthesize(R->Val, LHS, SMap, IC, depth, false));
    std::sort(Results.back().begin(), Results.back().end(),
              [](Inst *A, Inst *B) { return instCount(A) < instCount(B);});
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
      // Components.push_back(Builder(C, IC).BSwap()());
      Components.push_back(Builder(C, IC).LogB()());
      Components.push_back(Builder(C, IC).Sub(1)());
      Components.push_back(Builder(C, IC).Xor(-1)());
      if (SymbolizeHackersDelight) {
        Components.push_back(Builder(IC, llvm::APInt::getAllOnesValue(C->Width)).Shl(C)());
        Components.push_back(Builder(IC, llvm::APInt(C->Width, 1)).Shl(C)());
        Components.push_back(Builder(IC, C).BitWidth().Sub(1)());
        Components.push_back(Builder(IC, C).BitWidth().Sub(C)());
        // TODO: Add a few more, we can afford to run generalization longer
      }
    }

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

    // if (Cur->K == Inst::Const && Cur->Val == 0) {
    //   // Don't try to 'generalize' zero!
    //   Results.insert(Cur);
    // }

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
    if (DebugLevel > 2) {
      llvm::errs() << "POST " << Msg << "\n";
    }
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
    auto Name = "symconst_" + std::to_string(i++);
    if (I->Name != "") {
      Name = I->Name;
    }
    SymConstMap[I] = IC.createVar(I->Width, Name);

    InstCache[I] = SymConstMap[I];
    SymCS[SymConstMap[I]] = I->Val;
  }
  for (auto I : RHSConsts) {
    if (SymConstMap.find(I) != SymConstMap.end()) {
      continue;
    }
    auto Name = "symconst_" + std::to_string(i++);
    if (I->Name != "") {
      Name = I->Name;
    }
    SymConstMap[I] = IC.createVar(I->Width, Name);
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
  for (auto C : LHSConsts) {
    CommonConsts[C] = SymConstMap[C];
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

    if (Advanced) {
      Clone = SymKBPreconditionsAndVerifyGreedy(Result, IC, S, SymCS);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
    }

//    Clone = DFPreconditionsAndVerifyGreedy(Result, IC, S, SymCS);
//    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
//      return Clone;
//    }

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


  auto ConstantLimits = InferConstantLimits(CMap, IC, Input);


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
        Input.PCs.pop_back();
      }
    }
    Refresh("Relational constraints for enumerated cands.");
  }


  // Step 4.75 : Enumerate 2 instructions when single RHS Constant.
  std::vector<std::vector<Inst *>> EnumeratedCandidatesTwoInsts;
  if (RHSFresh.size() == 1) {
    EnumeratedCandidatesTwoInsts = Enumerate(RHSFresh, Components, IC, 2);

    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidatesTwoInsts,
                                  InstCache, IC, S, SymCS, true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Enumerated 2 insts");
  }

  if (!EnumeratedCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                  InstCache, IC, S, SymCS, false, true, true);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
  }
  Refresh("Enumerated exprs with constraints");

  if (RHSFresh.size() == 1) {
    // Enumerated Expressions with some relational constraints
    if (CMap.size() == 2) {
      for (auto &&R : Relations) {
        Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});
        auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidatesTwoInsts,
                                           InstCache, IC, S, SymCS, true, true, false);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return Clone;
        }
        Input.PCs.pop_back();
      }
    }
  }
  Refresh("Enumerated 2 insts exprs with relations");

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

  // Enumerated exprs with constraints

  if (!EnumeratedCandidates.empty()) {
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

  {
    if (!RHSFresh.empty()) {
      std::vector<std::vector<Inst *>> SimpleCandidatesMoreInsts =
        InferSpecialConstExprsAllSym(RHSFresh, LHSConsts, SymConstMap, IC, /*depth =*/ 5);

      if (!SimpleCandidates.empty()) {
        auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesMoreInsts,
                                          InstCache, IC, S, SymCS,
                                          true, false, false);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return Clone;
        }
      }

      Refresh("Special expressions, no constants");
    }
  }

  if (!EnumeratedCandidates.empty()) {
    for (auto &&R : ConstantLimits) {
      Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

      auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                         InstCache, IC, S, SymCS, true, true, false);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
      Input.PCs.pop_back();
    }
    Refresh("Enumerated expressions+consts and constant limits");
  }

  if (!SimpleCandidatesWithConsts.empty()) {
    for (auto &&R : ConstantLimits) {
      Input.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});

      auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                         InstCache, IC, S, SymCS, true, false, false);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
      Input.PCs.pop_back();
    }
    Refresh("Simple expressions+consts and constant limits");
  }

  }

  {
    auto Copy = Replace(Input, IC, JustLHSSymConstMap);
    for (auto &&R : ConstantLimits) {
      Copy.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});
      auto Clone = Verify(Copy, IC, S);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
      Copy.PCs.pop_back();
    }
    Refresh("Constant limit constraints on LHS");
  }
  if (SymbolicDF) {
  Refresh("PUSH SYMDF_DB");
  if (Input.Mapping.LHS->DemandedBits.getBitWidth() == Input.Mapping.LHS->Width &&
      !Input.Mapping.LHS->DemandedBits.isAllOnesValue()) {
    auto DB = Input.Mapping.LHS->DemandedBits;
    auto SymDFVar = IC.getConst(DB);
    SymDFVar->Name = "SYMDF_DB";

    SymDFVar->KnownOnes = llvm::APInt(DB.getBitWidth(), 0);
    SymDFVar->KnownZeros = llvm::APInt(DB.getBitWidth(), 0);
    SymDFVar->Val = DB;

    auto LHS = Input.Mapping.LHS;
    auto RHS = Input.Mapping.RHS;

    Input.Mapping.LHS->DemandedBits.setAllBits();
    Input.Mapping.RHS->DemandedBits.setAllBits();

    Input.Mapping.LHS = Builder(Input.Mapping.LHS, IC).And(SymDFVar)();
    Input.Mapping.RHS = Builder(Input.Mapping.RHS, IC).And(SymDFVar)();

    bool SymDFChanged = false;
    auto Generalized = SuccessiveSymbolize(IC, S, Input, SymDFChanged);

    if (SymDFChanged) {
      return Generalized;
    } else {
      Input.Mapping.LHS = LHS;
      Input.Mapping.RHS = RHS;
      Input.Mapping.LHS->DemandedBits = DB;
      Input.Mapping.RHS->DemandedBits = DB;
    }
  }
  Refresh("POP SYMDF_DB");
  Refresh("PUSH SYMDF_KB");
  {
    // Find good examples and test
    std::vector<Inst *> Inputs;
    findVars(Input.Mapping.LHS, Inputs);

    std::map<Inst *, APInt> KnownZero, KnownOne;
    auto PCs = Input.PCs;

    for (auto &&I : Inputs) {
      auto Width = I->Width;

      if (I->KnownZeros.getBitWidth() == I->Width &&
          I->KnownOnes.getBitWidth() == I->Width &&
          !(I->KnownZeros == 0 && I->KnownOnes == 0)) {
        KnownZero[I] = I->KnownZeros;
        KnownOne[I] = I->KnownOnes;

        if (I->KnownZeros != 0) {
          Inst *Zeros = IC.getConst(I->KnownZeros);
          Zeros->Name = "SYMDF_K0";
          Inst *AllOnes = IC.getConst(llvm::APInt::getAllOnesValue(Width));
          Inst *NotZeros = IC.getInst(Inst::Xor, Width,
                                  {Zeros, AllOnes});
          Inst *VarNotZero = IC.getInst(Inst::Or, Width, {I, NotZeros});
          Inst *ZeroBits = IC.getInst(Inst::Eq, 1, {VarNotZero, NotZeros});
          Input.PCs.push_back({ZeroBits, IC.getConst(llvm::APInt(1, 1))});
          I->KnownZeros = llvm::APInt(I->Width, 0);
        }

        if (I->KnownOnes != 0) {
          Inst *Ones = IC.getConst(I->KnownOnes);
          Ones->Name = "SYMDF_K1";
          Inst *VarAndOnes = IC.getInst(Inst::And, Width, {I, Ones});
          Inst *OneBits = IC.getInst(Inst::Eq, 1, {VarAndOnes, Ones});
          Input.PCs.push_back({OneBits, IC.getConst(llvm::APInt(1, 1))});
          I->KnownOnes = llvm::APInt(I->Width, 0);
        }
      }
    }

    if (!KnownZero.empty() || !KnownOne.empty()) {
      bool SymDFChanged = false;
      auto Generalized = SuccessiveSymbolize(IC, S, Input, SymDFChanged);
      if (SymDFChanged) {
        return Generalized;
      } else {
        Input.PCs = PCs;
        for (auto &&I : Inputs) {
          I->KnownZeros = KnownZero[I];
          I->KnownOnes = KnownOne[I];
        }
      }
    }
  }
  Refresh("POP SYMDF_KB");
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

InstMapping GetEqWidthConstraint(Inst *I, size_t Width, InstContext &IC) {
  return {Builder(I, IC).BitWidth().Eq(Width)(), IC.getConst(llvm::APInt(1, 1))};
}

InstMapping GetLessThanWidthConstraint(Inst *I, size_t Width, InstContext &IC) {
  // Don't need to check for >0.
  return {Builder(I, IC).BitWidth().Ule(Width)(), IC.getConst(llvm::APInt(1, 1))};
}
// TODO: More as needed.

Inst *CombinePCs(const std::vector<InstMapping> &PCs, InstContext &IC) {
  Inst *Ante = IC.getConst(llvm::APInt(1, true));
  for (auto PC : PCs ) {
    Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
  }
  return Ante;
}

ParsedReplacement InstantiateWidthChecks(InstContext &IC,
  Solver *S, ParsedReplacement Input) {
  // TODO: minimize width?

  // Instantiate Alive driver with Symbolic width.
  AliveDriver Alive(Input.Mapping.LHS,
  Input.PCs.empty() ? nullptr : CombinePCs(Input.PCs, IC),
  IC, {}, true);

  // Find set of valid widths.
  if (Alive.verify(Input.Mapping.RHS)) {
    if (DebugLevel > 4) {
      llvm::errs() << "WIDTH: Generalized opt is valid for all widths.\n";
    }
    // Completely width independent. No width checks needed.
    return Input;
  }

  auto &&ValidTypings = Alive.getValidTypings();

  if (ValidTypings.empty()) {
    // Something went wrong, generalized opt is not valid at any width.
    if (DebugLevel > 4) {
      llvm::errs() << "WIDTH: Generalized opt is not valid for any width.\n";
    }
    Input.Mapping.LHS = nullptr;
    Input.Mapping.RHS = nullptr;
    return Input;
  }

  // Abstract width to a range or relational precondition
  // TODO: Abstraction


  // If abstraction fails, insert checks for existing widths.
  std::vector<Inst *> Inputs;
  findVars(Input.Mapping.LHS, Inputs);
  for (auto &&I : Inputs) {
    Input.PCs.push_back(GetEqWidthConstraint(I, I->Width, IC));
  }
  return Input;
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
        size_t MaxTries = 1; // Increase this if we ever run with 10/100x timeout.
        do {
          if (!OnlyWidth) {
            Result = ReduceBasic(IC, S.get(), Input);
            Result = SuccessiveSymbolize(IC, S.get(), Result, Changed);
          }
          if (!NoWidth) {
            Result = InstantiateWidthChecks(IC, S.get(), Result);
          }
//          Result.print(llvm::errs(), true);
          if (!Result.Mapping.LHS) {
            break;
          }
        } while (--MaxTries && Changed);
      }
      if (Result.Mapping.LHS && Result.Mapping.RHS) {
        ReplacementContext RC;
        Result.printLHS(llvm::outs(), RC, true);
        Result.printRHS(llvm::outs(), RC, true);
        llvm::outs() << "\n";
      }
      continue;
    }


    if (FixIt) {
      // TODO: Implement fixit with existing functionality when needed
      llvm::errs() << "FixIt not implemented yet.\n";
    }
  }
  return 0;
}
