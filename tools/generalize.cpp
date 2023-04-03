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
#include <cstdlib>
#include <sstream>
#include <optional>

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

size_t InferWidth(Inst::Kind K, const std::vector<Inst *> &Ops) {
  switch (K) {
    case Inst::KnownOnesP:
    case Inst::KnownZerosP:
    case Inst::Eq:
    case Inst::Ne:
    case Inst::Slt:
    case Inst::Sle:
    case Inst::Ult:
    case Inst::Ule: return 1;
    case Inst::Select: return Ops[1]->Width;
    default: return Ops[0]->Width;
  }
}

struct InfixPrinter {
  InfixPrinter(ParsedReplacement P_, bool ShowImplicitWidths = true)
    : P(P_), ShowImplicitWidths(ShowImplicitWidths) {
    varnum = 0;
    std::vector<InstMapping> NewPCs;
    for (auto &&PC : P.PCs) {
      countUses(PC.LHS);
      countUses(PC.RHS);
      if (!registerSymDFVars(PC.LHS)) {
        NewPCs.push_back(PC);
      }
      countUses(P.Mapping.LHS);
      countUses(P.Mapping.RHS);
    }
    P.PCs = NewPCs;
    registerSymDBVar();
    registerWidthConstraints();
  }

  void registerWidthConstraints() {
    for (auto &&PC : P.PCs) {
      if (PC.LHS->K == Inst::Eq && PC.LHS->Ops[0]->K == Inst::BitWidth) {
        // PC.LHS looks like (width %x) == 32
        WidthConstraints[PC.LHS->Ops[0]->Ops[0]] = PC.LHS->Ops[1]->Val.getZExtValue();
      }
    }
  }

  void registerSymDBVar() {
    if (P.Mapping.LHS->K == Inst::DemandedMask) {
      Syms[P.Mapping.LHS->Ops[1]] = "@db";
      assert(P.Mapping.RHS->K == Inst::DemandedMask && "Expected RHS to be a demanded mask.");
      assert(P.Mapping.LHS->Ops[1] == P.Mapping.RHS->Ops[1] && "Expected same mask.");
      P.Mapping.LHS = P.Mapping.LHS->Ops[0];
      P.Mapping.RHS = P.Mapping.RHS->Ops[0];
    }
  }

  bool registerSymDFVars(Inst *I) {
    if (I->K == Inst::KnownOnesP && I->Ops[0]->K == Inst::Var &&
      I->Ops[1]->Name.starts_with("sym")) {
      Syms[I->Ops[1]] = I->Ops[0]->Name + ".k1";
      // VisitedVars.insert(I->Ops[1]->Name);
      return true;
    }
    if (I->K == Inst::KnownZerosP && I->Ops[0]->K == Inst::Var &&
      I->Ops[1]->Name.starts_with("sym")) {
      Syms[I->Ops[1]] = I->Ops[0]->Name + ".k0";
      // VisitedVars.insert(I->Ops[1]->Name);
      return true;
    }
    return false;
  }

  void countUses(Inst *I) {
    for (auto &&Op : I->Ops) {
      if (Op->K != Inst::Var && Op->K != Inst::Const) {
        UseCount[Op]++;
      }
      countUses(Op);
    }
  }

  template<typename Stream>
  void operator()(Stream &S) {
    if (!P.PCs.empty()) {
      printPCs(S);
      S << "\n  |= \n";
    }
    S << printInst(P.Mapping.LHS, S, true);
    if (!P.Mapping.LHS->DemandedBits.isAllOnesValue()) {
      S << " (" << "demandedBits="
       << Inst::getDemandedBitsString(P.Mapping.LHS->DemandedBits)
       << ")";
    }
    S << "\n  =>\n";

    S << printInst(P.Mapping.RHS, S, true) << "\n";
  }

  template<typename Stream>
  std::string printInst(Inst *I, Stream &S, bool Root = false) {
    if (Syms.count(I)) {
      return Syms[I];
    }

    std::ostringstream OS;

    if (UseCount[I] > 1) {
      std::string Name = "var" + std::to_string(varnum++);
      Syms[I] = Name;
      OS << "let " << Name << " = ";
    }

    // x ^ -1 => ~x
    if (I->K == Inst::Xor && I->Ops[1]->K == Inst::Const &&
        I->Ops[1]->Val.isAllOnesValue()) {
      return "~" + printInst(I->Ops[0], S);
    }
    if (I->K == Inst::Xor && I->Ops[0]->K == Inst::Const &&
        I->Ops[0]->Val.isAllOnesValue()) {
      return "~" + printInst(I->Ops[1], S);
    }

    if (I->K == Inst::Const) {
      if (I->Val.ule(16)) {
        return I->Val.toString(10, false);
      } else {
        return "0x" + I->Val.toString(16, false);
      }
    } else if (I->K == Inst::Var) {
      auto Name = I->Name;
      if (isDigit(Name[0])) {
        Name = "x" + Name;
      }
      if (I->Name.starts_with("symconst_")) {
        Name = "C" + I->Name.substr(9);
      }
      if (VisitedVars.count(I->Name)) {
        return Name;
      } else {
        VisitedVars.insert(I->Name);
        Inst::getKnownBitsString(I->KnownZeros, I->KnownOnes);

        std::string Buf;
        llvm::raw_string_ostream Out(Buf);

        if (I->KnownZeros.getBoolValue() || I->KnownOnes.getBoolValue())
          Out << " (knownBits=" << Inst::getKnownBitsString(I->KnownZeros, I->KnownOnes)
              << ")";
        if (I->NonNegative)
          Out << " (nonNegative)";
        if (I->Negative)
          Out << " (negative)";
        if (I->NonZero)
          Out << " (nonZero)";
        if (I->PowOfTwo)
          Out << " (powerOfTwo)";
        if (I->NumSignBits > 1)
          Out << " (signBits=" << I->NumSignBits << ")";
        if (!I->Range.isFullSet())
          Out << " (range=[" << I->Range.getLower()
              << "," << I->Range.getUpper() << "))";

        std::string W = ShowImplicitWidths ? ":i" + std::to_string(I->Width) : "";

        if (WidthConstraints.count(I)) {
          W = ":i" + std::to_string(WidthConstraints[I]);
        }

        return Name + W + Out.str();
      }
    } else {
      std::string Op;
      switch (I->K) {
      case Inst::Add: Op = "+"; break;
      case Inst::AddNSW: Op = "+nsw"; break;
      case Inst::AddNUW: Op = "+nuw"; break;
      case Inst::AddNW: Op = "+nw"; break;
      case Inst::Sub: Op = "-"; break;
      case Inst::SubNSW: Op = "-nsw"; break;
      case Inst::SubNUW: Op = "-nuw"; break;
      case Inst::SubNW: Op = "-nw"; break;
      case Inst::Mul: Op = "*"; break;
      case Inst::MulNSW: Op = "*nsw"; break;
      case Inst::MulNUW: Op = "*nuw"; break;
      case Inst::MulNW: Op = "*nw"; break;
      case Inst::UDiv: Op = "/u"; break;
      case Inst::SDiv: Op = "/s"; break;
      case Inst::URem: Op = "\%u"; break;
      case Inst::SRem: Op = "\%s"; break;
      case Inst::And: Op = "&"; break;
      case Inst::Or: Op = "|"; break;
      case Inst::Xor: Op = "^"; break;
      case Inst::Shl: Op = "<<"; break;
      case Inst::ShlNSW: Op = "<<nsw"; break;
      case Inst::ShlNUW: Op = "<<nuw"; break;
      case Inst::ShlNW: Op = "<<nw"; break;
      case Inst::LShr: Op = ">>l"; break;
      case Inst::AShr: Op = ">>a"; break;
      case Inst::Eq: Op = "=="; break;
      case Inst::Ne: Op = "!="; break;
      case Inst::Ult: Op = "<u"; break;
      case Inst::Slt: Op = "<s"; break;
      case Inst::Ule: Op = "<=u"; break;
      case Inst::Sle: Op = "<=s"; break;
      default: Op = Inst::getKindName(I->K); break;
      }

      std::string Result;

      std::vector<Inst *> Ops = I->orderedOps();

      if (Inst::isCommutative(I->K)) {
        std::sort(Ops.begin(), Ops.end(), [](Inst *A, Inst *B) {
          if (A->K == Inst::Const) {
            return false; // c OP expr
          } else if (B->K == Inst::Const) {
            return true; // expr OP c
          } else if (A->K == Inst::Var && B->K != Inst::Var) {
            return true; // var OP expr
          } else if (A->K != Inst::Var && B->K == Inst::Var) {
            return false; // expr OP var
          } else if (A->K == Inst::Var && B->K == Inst::Var) {
            return A->Name > B->Name; // Tends to put vars before symconsts
          } else {
            return A->K < B->K; // expr OP expr
          }
        });
      }

      if (Ops.size() == 2) {
        auto Meat = printInst(Ops[0], S) + " " + Op + " " + printInst(Ops[1], S);
        Result = Root ? Meat : "(" + Meat + ")";
      } else if (Ops.size() == 1) {
        Result = Op + "(" + printInst(Ops[0], S) + ")";
      }
      else {
        std::string Ret = Root ? "" : "(";
        Ret += Op;
        Ret += " ";
        for (auto &&Op : Ops) {
          Ret += printInst(Op, S) + " ";
        }
        while (Ret.back() == ' ') {
          Ret.pop_back();
        }
        if (!Root) {
          Ret += ")";
        }
        Result = Ret;
      }
      if (UseCount[I] > 1) {
        OS << Result << ";\n";
        S << OS.str();
        return Syms[I];
      } else {
        return Result;
      }
    }
  }

  template<typename Stream>
  void printPCs(Stream &S) {
    bool first = true;
    for (auto &&PC : P.PCs) {
      // if (PC.LHS->K == Inst::KnownOnesP || PC.LHS->K == Inst::KnownZerosP) {
      //   continue;
      // }
      if (first) {
        first = false;
      } else {
        S << " && \n";
      }
      if (PC.RHS->K == Inst::Const && PC.RHS->Val == 0) {
        S << "!(" << printInst(PC.LHS, S, true) << ")";
      } else if (PC.RHS->K == Inst::Const && PC.RHS->Val == 1) {
        S << printInst(PC.LHS, S, true);
      } else {
        S << printInst(PC.LHS, S, true) << " == " << printInst(PC.RHS, S);
      }
    }
  }

  ParsedReplacement P;
  std::set<std::string> VisitedVars;
  std::map<Inst *, std::string> Syms;
  size_t varnum;
  std::map<Inst *, size_t> UseCount;
  std::map<Inst *, size_t> WidthConstraints;
  bool ShowImplicitWidths;
};

using ConstMapT = std::vector<std::pair<Inst *, llvm::APInt>>;
std::pair<ConstMapT, ParsedReplacement>
AugmentForSymKBDB(ParsedReplacement Original, InstContext &IC) {
  auto Input = Clone(Original, IC);
  std::vector<std::pair<Inst *, llvm::APInt>> ConstMap;
  if (Input.Mapping.LHS->DemandedBits.getBitWidth() == Input.Mapping.LHS->Width &&
    !Input.Mapping.LHS->DemandedBits.isAllOnesValue()) {
    auto DB = Input.Mapping.LHS->DemandedBits;
    auto SymDFVar = IC.createVar(DB.getBitWidth(), "symDF_DB");
    // SymDFVar->Name = "symDF_DB";

    SymDFVar->KnownOnes = llvm::APInt(DB.getBitWidth(), 0);
    SymDFVar->KnownZeros = llvm::APInt(DB.getBitWidth(), 0);
    // SymDFVar->Val = DB;

    Input.Mapping.LHS->DemandedBits.setAllBits();
    Input.Mapping.RHS->DemandedBits.setAllBits();

    auto W = Input.Mapping.LHS->Width;

    Input.Mapping.LHS = IC.getInst(Inst::DemandedMask, W, {Input.Mapping.LHS, SymDFVar});
    Input.Mapping.RHS = IC.getInst(Inst::DemandedMask, W, {Input.Mapping.RHS, SymDFVar});

    ConstMap.push_back({SymDFVar, DB});
  }

  std::vector<Inst *> Inputs;
  findVars(Input.Mapping.LHS, Inputs);

  for (auto &&I : Inputs) {
    auto Width = I->Width;
    if (I->KnownZeros.getBitWidth() == I->Width &&
        I->KnownOnes.getBitWidth() == I->Width &&
        !(I->KnownZeros == 0 && I->KnownOnes == 0)) {
      if (I->KnownZeros != 0) {
        Inst *Zeros = IC.createVar(Width, "symDF_K0");

        // Inst *AllOnes = IC.getConst(llvm::APInt::getAllOnesValue(Width));
        // Inst *NotZeros = IC.getInst(Inst::Xor, Width,
        //                         {Zeros, AllOnes});
        // Inst *VarNotZero = IC.getInst(Inst::Or, Width, {I, NotZeros});
        // Inst *ZeroBits = IC.getInst(Inst::Eq, 1, {VarNotZero, NotZeros});
        Inst *ZeroBits = IC.getInst(Inst::KnownZerosP, 1, {I, Zeros});
        Input.PCs.push_back({ZeroBits, IC.getConst(llvm::APInt(1, 1))});
        ConstMap.push_back({Zeros, I->KnownZeros});
        I->KnownZeros = llvm::APInt(I->Width, 0);
      }

      if (I->KnownOnes != 0) {
        Inst *Ones = IC.createVar(Width, "symDF_K1");
        // Inst *VarAndOnes = IC.getInst(Inst::And, Width, {I, Ones});
        // Inst *OneBits = IC.getInst(Inst::Eq, 1, {VarAndOnes, Ones});
        Inst *OneBits = IC.getInst(Inst::KnownOnesP, 1, {I, Ones});
        Input.PCs.push_back({OneBits, IC.getConst(llvm::APInt(1, 1))});
        ConstMap.push_back({Ones, I->KnownOnes});
        I->KnownOnes = llvm::APInt(I->Width, 0);
      }
    }
  }

  return {ConstMap, Input};
}

bool typeCheck(Inst *I) {
  if (I->Ops.size() == 2) {
    if (I->Ops[0]->Width != I->Ops[1]->Width) {
      if (DebugLevel > 4) llvm::errs() << "Operands must have the same width\n";
      return false;
    }
  }
  if (I->K == Inst::Select) {
    if (I->Ops[0]->Width != 1) {
      if (DebugLevel > 4) llvm::errs() << "Select condition must be 1 bit wide\n";
      return false;
    }
    if (I->Ops[1]->Width != I->Ops[2]->Width) {
      if (DebugLevel > 4) llvm::errs() << "Select operands must have the same width\n";
      return false;
    }
  }
  if (Inst::isCmp(I->K)) {
    if (I->Width != 1) {
      if (DebugLevel > 4) llvm::errs() << "Comparison must be 1 bit wide\n";
      return false;
    }
  }
  if (I->K == Inst::Trunc) {
    if (I->Ops[0]->Width <= I->Width) {
      if (DebugLevel > 4) llvm::errs() << "Trunc operand must be wider than result\n";
      return false;
    }
  }
  if (I->K == Inst::ZExt || I->K == Inst::SExt) {
    if (I->Ops[0]->Width >= I->Width) {
      if (DebugLevel > 4) llvm::errs() << "Ext operand must be narrower than result\n";
      return false;
    }
  }
  return true;
}
bool typeCheck(ParsedReplacement &R) {
  if (R.Mapping.LHS->Width != R.Mapping.RHS->Width) {
    if (DebugLevel > 4) llvm::errs() << "LHS and RHS must have the same width\n";
    return false;
  }

  if (!typeCheck(R.Mapping.LHS)) {
    return false;
  }
  if (!typeCheck(R.Mapping.RHS)) {
    return false;
  }

  for (auto &&PC : R.PCs) {
    if (!typeCheck(PC.LHS)) {
      return false;
    }
    if (!typeCheck(PC.RHS)) {
      return false;
    }
  }
  return true;
}

struct ShrinkWrap {
  ShrinkWrap(InstContext &IC, Solver *S, ParsedReplacement Input,
             size_t TargetWidth = 8) : IC(IC), S(S), Input(Input),
                                       TargetWidth(TargetWidth) {}
  InstContext &IC;
  Solver *S;
  ParsedReplacement Input;
  size_t TargetWidth;

  std::map<Inst *, Inst *> InstCache;

  Inst *ShrinkInst(Inst *I, Inst *Parent, size_t ResultWidth) {
    if (InstCache.count(I)) {
      return InstCache[I];
    }
    if (I->K == Inst::Var) {
      if (I->Width == 1) {
        return I;
      }
      auto V = IC.createVar(ResultWidth, I->Name);
      InstCache[I] = V;
      return V;
    } else if (I->K == Inst::Const) {
      if (I->Width == 1) {
        return I;
      }
      // Treat 0, 1, and -1 specially
      if (I->Val.getLimitedValue() == 0) {
        auto C = IC.getConst(APInt(ResultWidth, 0));
        InstCache[I] = C;
        return C;
      } else if (I->Val.getLimitedValue() == 1) {
        auto C = IC.getConst(APInt(ResultWidth, 1));
        InstCache[I] = C;
        return C;
      } else if (I->Val.isAllOnesValue()) {
        auto C = IC.getConst(APInt::getAllOnesValue(ResultWidth));
        InstCache[I] = C;
        return C;
      } else {
        auto C = IC.createSynthesisConstant(ResultWidth, I->Val.getLimitedValue());
        InstCache[I] = C;
        return C;
      }
    } else {
      if (I->K == Inst::Trunc) {
        size_t Target = 0;
        // llvm::errs() << "HERE: " << I->Width << " " << I->Ops[0]->Width << '\n';
        if (I->Ops[0]->Width == I->Width + 1) {
          // llvm::errs() << "a\n";
          Target = ResultWidth + 1;
        } else if (I->Ops[0]->Width == 2 * I->Width) {
          Target = ResultWidth * 2;
          // llvm::errs() << "b\n";
        } else if (I->Width == 1 && I->Ops[0]->Width != 1) {
          // llvm::errs() << "c\n";
          Target = TargetWidth;
          ResultWidth = 1;
        } else {
          // Maintain ratio
          // llvm::errs() << "d\n";
          Target = ResultWidth * I->Ops[0]->Width * 1.0 / I->Width;
        }
        // llvm::errs() << "HERE: " << ResultWidth << " " << Target << '\n';
        return IC.getInst(Inst::Trunc, ResultWidth, { ShrinkInst(I->Ops[0], I, Target)});
      }
      if (I->K == Inst::ZExt || I->K == Inst::SExt) {
        size_t Target = 0;
        if (I->Ops[0]->Width == I->Width - 1) {
          Target = ResultWidth - 1;
        } else if (I->Ops[0]->Width == I->Width / 2) {
          Target = ResultWidth / 2;
        } else if (I->Ops[0]->Width == 1) {
          Target = 1;
        } else {
          // Maintain ratio
          Target = ResultWidth * I->Ops[0]->Width * 1.0 / I->Width;
        }
        return IC.getInst(I->K, ResultWidth, { ShrinkInst(I->Ops[0], I, Target)});
      }

      if (I->K == Inst::Eq || I->K == Inst::Ne ||
          I->K == Inst::Ult || I->K == Inst::Slt ||
          I->K == Inst::Ule || I->K == Inst::Sle ||
          I->K == Inst::KnownOnesP || I->K == Inst::KnownZerosP) {
        ResultWidth = TargetWidth;
      }

      std::vector<Inst *> Ops;
      for (auto Op : I->Ops) {
        Ops.push_back(ShrinkInst(Op, I, ResultWidth));
      }
      return IC.getInst(I->K, InferWidth(I->K, Ops), Ops);
    }
  }

  std::optional<ParsedReplacement> operator()() {

    auto [CM, Aug] = AugmentForSymKBDB(Input, IC);

    if (!CM.empty()) {
      Input = Aug;
    }

    // Abort if inputs are of <= Target width
    std::vector<Inst *> Inputs;
    // TODO: Is there a better decision here?
    findVars(Input.Mapping.LHS, Inputs);
    for (auto I : Inputs) {
      if (I->Width <= TargetWidth) {
        return {};
      }
      if (!I->Range.isFullSet()) {
        return {};
      }
    }

    ParsedReplacement New;
    New.Mapping.LHS = ShrinkInst(Input.Mapping.LHS, nullptr, TargetWidth);
    New.Mapping.RHS = ShrinkInst(Input.Mapping.RHS, nullptr, TargetWidth);
    for (auto PC : Input.PCs) {
      New.PCs.push_back({ShrinkInst(PC.LHS, nullptr, TargetWidth),
                         ShrinkInst(PC.RHS, nullptr, TargetWidth)});
    }

    // New.print(llvm::errs(), true);
    if (!typeCheck(New)) {
      llvm::errs() << "Type check failed\n";
      return {};
    }
    auto Clone = Verify(New, IC, S);
    if (Clone.Mapping.LHS) {
      return Clone;
    } else {
      return {};
    }
  }
};

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

std::vector<Inst *> FilterExprsByValue(const std::vector<Inst *> &Exprs,
  llvm::APInt TargetVal, const std::vector<std::pair<Inst *, llvm::APInt>> &CMap) {
  std::unordered_map<Inst *, EvalValue> ValueCache;
  for (auto &&[I, V] : CMap) {
    ValueCache[I] = EvalValue(V);
  }
  std::vector<Inst *> FilteredExprs;
  ConcreteInterpreter CPos(ValueCache);
  for (auto &&E : Exprs) {
    auto Result = CPos.evaluateInst(E);
    if (!Result.hasValue()) {
      // Don't want to drop a candidate just because we couldn't evaluate it
      FilteredExprs.push_back(E);
    } else {
      if (Result.getValue() == TargetVal) {
        FilteredExprs.push_back(E);
      }
    }
  }
  return FilteredExprs;
}

std::vector<Inst *> FilterRelationsByValue(const std::vector<Inst *> &Relations,
                        const std::vector<std::pair<Inst *, llvm::APInt>> &CMap,
                        std::vector<ValueCache> CEXs) {
  std::unordered_map<Inst *, EvalValue> ValueCache;
  for (auto &&[I, V] : CMap) {
    ValueCache[I] = EvalValue(V);
  }

  ConcreteInterpreter CPos(ValueCache);
  std::vector<ConcreteInterpreter> CNegs;
  for (auto &&CEX : CEXs) {
    CNegs.push_back(CEX);
  }

  std::vector<Inst *> FilteredRelations;
  for (auto &&R : Relations) {
    auto Result = CPos.evaluateInst(R);
    // Positive example
    if (Result.hasValue() && !Result.getValue().isAllOnesValue()) {
      continue;
    }

    // Negative examples
    bool foundUnsound = false;
    for (auto &&CNeg : CNegs) {
      auto ResultNeg = CNeg.evaluateInst(R);
      if (ResultNeg.hasValue() && !ResultNeg.getValue().isNullValue()) {
        foundUnsound = true;
        break;
      }
    }
    if (foundUnsound) {
      continue;
    }
    FilteredRelations.push_back(R);
  }
  return FilteredRelations;
}

std::vector<Inst *> InferConstantLimits(
  const std::vector<std::pair<Inst *, llvm::APInt>> &CMap,
        InstContext &IC, const ParsedReplacement &Input,
        std::vector<ValueCache> CEXs) {
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
    // Results.push_back(Builder(XI, IC).Ult(Width)());
    // Results.push_back(Builder(XI, IC).Ule(Width)());

    // X slt SMAX, x ult UMAX
    auto WM1 = Width.Sub(1);
    auto SMax = Builder(IC, llvm::APInt(XI->Width, 1)).Shl(WM1).Sub(1)();
    Results.push_back(Builder(XI, IC).Slt(SMax)());

    // auto gZ = Builder(XI, IC).Ugt(0)();

    // Results.push_back(Builder(XI, IC).Ult(Width).And(gZ)());
    // Results.push_back(Builder(XI, IC).Ule(Width).And(gZ)());

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
      if (XI->Width != YI->Width) {
        continue;
      }
      auto Sum = Builder(XI, IC).Add(YI)();
      // // Sum related to width
      // auto Width = Builder(Sum, IC).BitWidth();
      // Results.push_back(Builder(Sum, IC).Ult(Width)());
      // Results.push_back(Builder(Sum, IC).Ule(Width)());
      // Results.push_back(Builder(Sum, IC).Eq(Width)());

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
  return FilterRelationsByValue(Results, CMap, CEXs);
}

// Enforce commutativity to prune search space
bool comm(Inst *A, Inst *B, Inst *C) {
  return A > B && B > C;
}
bool comm(Inst *A, Inst *B) {
  return A > B;
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
#define C2 comm(XI, YI)
#define C3 comm(XI, YI, ZI)

std::vector<Inst *> InferPotentialRelations(
        const std::vector<std::pair<Inst *, llvm::APInt>> &CMap,
        InstContext &IC, const ParsedReplacement &Input, std::vector<ValueCache> CEXs,
        bool LatticeChecks = false) {
  std::vector<Inst *> Results;
  if (!FindConstantRelations) {
    return Results;
  }


  // if (DebugLevel) {
  //   llvm::errs() << "Symconsts for rels: " << CMap.size() << "\n";
  // }
  // Triple rels
  if (CMap.size() >= 3) {
    for (auto &&[XI, XC] : CMap) {
      for (auto &&[YI, YC] : CMap) {
        for (auto &&[ZI, ZC] : CMap) {
          if (XI == YI || XI == ZI || YI == ZI) {
            continue;
          }
          if (XC.getBitWidth() != YC.getBitWidth() ||
              XC.getBitWidth() != ZC.getBitWidth()) {
            continue;
          }

          if (C3 && (XC | YC | ZC).isAllOnesValue()) {
            Results.push_back(Builder(XI, IC).Or(YI).Or(ZI)
              .Eq(llvm::APInt::getAllOnesValue(XI->Width))());
          }

          if (C3 && (XC & YC & ZC) == 0) {
            Results.push_back(Builder(XI, IC).And(YI).And(ZI)
              .Eq(llvm::APInt(XI->Width, 0))());
          }

          // TODO Make width independent by using bitwidth insts
          if (C2 && (XC | YC | ~ZC).isAllOnesValue()) {
            Results.push_back(Builder(XI, IC).Or(YI).Or(Builder(ZI, IC).Flip())
              .Eq(llvm::APInt::getAllOnesValue(XI->Width))());
          }

          if (XC << YC == ZC) {
            Results.push_back(Builder(XI, IC).Shl(YI).Eq(ZI)());
          }

          if (XC.lshr(YC) == ZC) {
            Results.push_back(Builder(XI, IC).LShr(YI).Eq(ZI)());
          }

          // if (C2 && (XC & YC).eq(ZC)) {
          //   Results.push_back(Builder(XI, IC).And(YI).Eq(ZI)());
          // }

          // if (C2 && (XC | YC).eq(ZC)) {
          //   Results.push_back(Builder(XI, IC).Or(YI).Eq(ZI)());
          // }

          // if (C2 && (XC ^ YC).eq(ZC)) {
          //   Results.push_back(Builder(XI, IC).Xor(YI).Eq(ZI)());
          // }

          // if (C2 && (XC != 0 && YC != 0) && (XC + YC).eq(ZC)) {
          //   Results.push_back(Builder(XI, IC).Add(YI).Eq(ZI)());
          // }

        }
      }
    }
  }

  // Pairwise relations
  for (auto &&[XI, XC] : CMap) {
    // llvm::errs() << "HERE: " << XC << "\n";
    for (auto &&[YI, YC] : CMap) {
      if (XI == YI || XC.getBitWidth() != YC.getBitWidth()) {
        continue;
      }

      if (~XC == YC) {
        Results.push_back(Builder(XI, IC).Flip().Eq(YI)());
      }

      // if (C2 && XC == YC) {
      //   Results.push_back(Builder(XI, IC).Eq(YI)());
      // }

      // if ((XC & YC) == XC) {
      //   Results.push_back(Builder(XI, IC).And(YI).Eq(XI)());

      // }

      // if ((XC & YC) == YC) {
      //   auto W = XI->Width;
      //   Results.push_back(IC.getInst(Inst::KnownOnesP, W, {XI, YI}));
      // }

      // TODO guard
      // Results.back()->Print();

      // Results.push_back(IC.getInst(Inst::KnownZerosP, W, {XI, YI}));

      // todo knownzerosp

      // if ((XC | YC) == XC) {
      //   Results.push_back(Builder(XI, IC).Or(YI).Eq(XI)());
      // }

      // if ((XC | YC) == YC) {
      //   Results.push_back(Builder(XI, IC).Or(YI).Eq(YI)());
      // }

      // Mul C
      if (C2 && YC!= 0 && XC.urem(YC) == 0) {
        auto Fact = XC.udiv(YC);
        if (Fact != 1) {
          Results.push_back(Builder(YI, IC).Mul(Fact).Eq(XI)());
        }
      }

      // Add C
      // auto Diff = XC - YC;
      // if (Diff != 0) {
      //   Results.push_back(Builder(XI, IC).Sub(Diff).Eq(YI)());
      // }

      if (C2 && XC != 0 && YC.urem(XC) == 0) {
        auto Fact = YC.udiv(XC);
        if (Fact != 1) {
          Results.push_back(Builder(XI, IC).Mul(Fact).Eq(YI)());
        }
      }

      // // TODO Check if this is too slow
      // // if (Input.Mapping.LHS->Width == 1) {
      //   // need both signed and unsigned?
      //   // What about s/t/e/ versions?
      //   if (XC.slt(YC)) Results.push_back(Builder(XI, IC).Slt(YI)());
      //   if (XC.ult(YC)) Results.push_back(Builder(XI, IC).Ult(YI)());
      //   if (YC.slt(XC)) Results.push_back(Builder(YI, IC).Slt(XI)());
      //   if (YC.ult(XC)) Results.push_back(Builder(YI, IC).Ult(XI)());
      // // }

      // auto XBits = BitFuncs(XI, IC);
      // auto YBits = BitFuncs(YI, IC);

      // for (auto &&XBit : XBits) {
      //   for (auto &&YBit : YBits) {
      //     Results.push_back(Builder(XBit, IC).Ule(YBit)());
      //     Results.push_back(Builder(XBit, IC).Ult(YBit)());
      //   }
      // }

      // No example yet where this is useful
      // for (auto &&XBit : XBits) {
      //   for (auto &&YBit : YBits) {
      //     Results.push_back(Builder(XBit, IC).Ne(YBit)());
      //     Results.push_back(Builder(XBit, IC).Eq(YBit)());
      //   }
      // }

    }
    Results.push_back(Builder(XI, IC).Eq(Builder(XI, IC).BitWidth().Sub(1))());
    // Results.push_back(Builder(XI, IC).Eq(Builder(XI, IC).BitWidth().UDiv(2))());
    // Results.push_back(Builder(XI, IC).Eq(Builder(XI, IC).BitWidth())());
  }

  // TODO: Make sure this works.
  for (auto &&[XI, XC] : CMap) {
    for (auto &&[YI, YC] : CMap) {
      if (XI == YI || XC.getBitWidth() == YC.getBitWidth()) {
        continue;
      }

      // llvm::errs() << "HERE: " << XI->Name << ' ' << YI->Name << ' ' << XC.getLimitedValue() << ' ' <<  YC.getLimitedValue() << '\n';

      // llvm::errs() << "HERE: " << XC.getLimitedValue() << ' ' <<  YC.getLimitedValue() << '\n';
      if (XC.getLimitedValue() == YC.getLimitedValue()) {
        if (XI->Width > YI->Width) {
          // Builder(YI, IC).ZExt(XI->Width).Eq(XI)()->Print();
          Results.push_back(Builder(YI, IC).ZExt(XI->Width).Eq(XI)());
        } else {
          Results.push_back(Builder(XI, IC).ZExt(YI->Width).Eq(YI)());
        }
      }
    }
  }

  // for (auto R : InferConstantLimits(CMap, IC, Input)) {
  //   Results.push_back(R);
  // }
  // llvm::errs() << "HERE: " << Results.size() << '\n';
  Results = FilterRelationsByValue(Results, CMap, CEXs);

  if (LatticeChecks) {
    // TODO Less brute force
    for (auto &&[XI, XC] : CMap) {
      for (auto &&[YI, YC] : CMap) {
        if (XI == YI || XC.getBitWidth() != YC.getBitWidth()) {
          continue;
        }
        Results.push_back(IC.getInst(Inst::KnownOnesP, 1, {XI, YI}));
        Results.push_back(IC.getInst(Inst::KnownZerosP, 1, {XI, YI}));
      }
    }
  }

  return Results;
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

size_t BruteForceModelCount(Inst *Pred) {
  if (Pred->Width >= 8) {
    llvm::errs() << "Too wide for brute force model counting.\n";
    return 0;
  }

  std::vector<Inst *> Inputs;
  findVars(Pred, Inputs);

  ValueCache Cache;
  for (auto I : Inputs) {
    Cache[I] = EvalValue(llvm::APInt(I->Width, 0));
  }

  auto Update = [&]() {
    for (auto I : Inputs) {
      if (Cache[I].getValue() == llvm::APInt(I->Width, -1)) {
        continue;
      } else {
        Cache[I] = EvalValue(Cache[I].getValue() + 1);
        return true;
      }
    }
    return false;
  };

  size_t ModelCount = 0;

  do {
    ConcreteInterpreter CI(Cache);
    if (CI.evaluateInst(Pred).getValue().getBoolValue()) {
      ++ModelCount;
    }
  } while (Update());

  return ModelCount;
}

void SortPredsByModelCount(std::vector<Inst *> &Preds) {
  std::unordered_map<Inst *, size_t> ModelCounts;
  for (auto P : Preds) {
    ModelCounts[P] = BruteForceModelCount(P);
  }
  std::sort(Preds.begin(), Preds.end(), [&](Inst *A, Inst *B) {
    return ModelCounts[A] > ModelCounts[B];
  });
}

std::optional<ParsedReplacement> VerifyWithRels(InstContext &IC, Solver *S,
                                 ParsedReplacement Input,
                                 std::vector<Inst *> &Rels) {
  std::vector<Inst *> ValidRels;

  ParsedReplacement FirstValidResult = Input;

  for (auto Rel : Rels) {
    Input.PCs.push_back({Rel, IC.getConst(llvm::APInt(1, 1))});
    auto Clone = Verify(Input, IC, S);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      ValidRels.push_back(Rel);
      FirstValidResult = Clone;
      if (Rels.size() > 10) {
        return Clone;
      }
    }
    Input.PCs.pop_back();
  }

  if (ValidRels.empty()) {
    return std::nullopt;
  }

  std::vector<Inst *> Inputs;
  findVars(Input.Mapping.LHS, Inputs);

  size_t MaxWidth = 0;
  for (auto I : Inputs) {
    if (I->Width > MaxWidth) {
      MaxWidth = I->Width;
    }
  }

  if (MaxWidth > 8) {
    if (DebugLevel > 4) {
      llvm::errs() << "Too wide for brute force model counting.\n";
    }
    // TODO: Use approximate model counting?
    return FirstValidResult;
  }

  SortPredsByModelCount(ValidRels);
  // TODO: Construct WP
  // For now, return the weakest valid result
  Input.PCs.push_back({ValidRels[0], IC.getConst(llvm::APInt(1, 1))});
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
                      bool DFF,
                      std::vector<Inst *> Rels = {}) {
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
      // InfixPrinter IP(P);
      // IP(llvm::errs());
      // llvm::errs() << "\n";

      if (GEN) {
        Clone = Verify(P, IC, S);
        if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
          return true;
        }
      }

      if (!Rels.empty()) {
        auto Result = VerifyWithRels(IC, S, P, Rels);
        if (Result) {
          Clone = *Result;
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

    // Copy.print(llvm::errs(), true);
    // llvm::errs() << "\n";

    if (SOLVE(Copy)) {
      return Clone;
    }

    if (!ReverseMap.empty()) {
      Copy.Mapping.LHS = Replace(Copy.Mapping.LHS, IC, ReverseMap);
      Copy.Mapping.RHS = Replace(Copy.Mapping.RHS, IC, ReverseMap);
      if (SOLVE(Copy)) {
        return Clone;
      }
    }

  }

  Input.Mapping.LHS = nullptr;
  Input.Mapping.RHS = nullptr;
  return Input;
}



std::vector<Inst *> IOSynthesize(llvm::APInt Target,
const std::vector<std::pair<Inst *, llvm::APInt>> &ConstMap,
InstContext &IC, size_t Threshold, bool ConstMode, Inst *ParentConst = nullptr) {

  std::vector<Inst *> Results;

  // Handle width changes
  for (const auto &[I, Val] : ConstMap) {
    if (Target.getBitWidth() == I->Width || !Threshold ) {
      continue;
    }

    llvm::APInt NewTarget = Target;
    if (Target.getBitWidth() < I->Width) {
      NewTarget = Target.sgt(0) ? Target.zext(I->Width) : Target.sext(I->Width);
    } else {
      NewTarget = Target.trunc(I->Width);
    }
    for (auto X : IOSynthesize(NewTarget, ConstMap, IC, Threshold - 1, ConstMode, nullptr)) {
      // ReplacementContext RC;
      // RC.printInst(X, llvm::errs(), true);
      if (I->Width < X->Width) {
        Results.push_back(Builder(IC, X).ZExt(Target.getBitWidth())());
        Results.push_back(Builder(IC, X).SExt(Target.getBitWidth())());
      } else {
        Results.push_back(Builder(IC, X).Trunc(Target.getBitWidth())());
      }
    }
  }

  for (const auto &[I, Val] : ConstMap) {
    if (I == ParentConst) {
      continue;
    }
    if (I->Width != Target.getBitWidth()) {
      continue;
    }
    if (!ConstMode) {
      if (Val == Target) {
        Results.push_back(I);
      } else if (Val == 0 - Target) {
        Results.push_back(Builder(IC, I).Negate()());
      } else if (Val == ~Target) {
        Results.push_back(Builder(IC, I).Flip()());
      }

      // llvm::errs() << "Trying to synthesize " << Target << " from " << Val << "\n";

      auto One = llvm::APInt(I->Width, 1);
      // llvm::errs() << "1: " << One.shl(Val) << "\n";
      if (One.shl(Val) == Target) {
        Results.push_back(Builder(IC, One).Shl(I)());
      }
      auto MinusOneVal = llvm::APInt::getAllOnesValue(I->Width);

      auto OneBitOne = llvm::APInt(1, 1);
      auto MinusOne = Builder(IC, OneBitOne).SExt(I->Width)();

      // llvm::errs() << "2: " << MinusOne.shl(Val) << "\n";
      if (MinusOneVal.shl(Val) == Target) {
        Results.push_back(Builder(IC, MinusOne).Shl(I)());
      }
      // llvm::errs() << "3: " << MinusOne.lshr(Val) << "\n";
      if (MinusOneVal.lshr(Val) == Target) {
        Results.push_back(Builder(IC, MinusOne).LShr(I)());
      }
    } else {
      if (ParentConst) {
        Results.push_back(Builder(IC, Target)());
      }
    }
  }

  if (!Threshold) {
    return Results;
  }

  // Recursive formulation

  #define for_no_nop(X, x) \
  if (Target != x) for (auto X : \
  IOSynthesize(x, ConstMap, IC, Threshold - 1, ConstMode, ParentConst))

  for (const auto &[I, Val] : ConstMap) {
    if (I->Width != Target.getBitWidth()) {
      continue;
    }

    if (I == ParentConst) {
      continue;
    }
    ParentConst = I;

    // Binary operators

    // C + X == Target
    for_no_nop(X, Target - Val) {
      Results.push_back(Builder(I, IC).Add(X)());
    }

    // C - X == Target
    for_no_nop(X, Val - Target) {
      Results.push_back(Builder(I, IC).Sub(X)());
    }

    // X - C == Target
    for_no_nop(X, Target + Val) {
      Results.push_back(Builder(X, IC).Sub(I)());
    }

    // C * X == Target
    if (Val.isNegative() || Target.isNegative()) {
      if (Val != 0 && Target.srem(Val) == 0) {
        for_no_nop(X, Target.sdiv(Val)) {
          Results.push_back(Builder(X, IC).Mul(I)());
        }
      }
    } else {
      if (Val != 0 && Target.urem(Val) == 0) {
        for_no_nop(X, Target.udiv(Val)) {
          Results.push_back(Builder(X, IC).Mul(I)());
        }
      }
    }

    // C / X == Target
    if (Val.isNegative() || Target.isNegative()) {
      if (Target != 0 && Val.srem(Target) == 0) {
        for_no_nop(X, Val.sdiv(Target)) {
          Results.push_back(Builder(I, IC).SDiv(X)());
        }
      }
    } else {
      if (Target != 0 && Val.urem(Target) == 0) {
        for_no_nop(X, Val.udiv(Target)) {
          Results.push_back(Builder(I, IC).UDiv(X)());
        }
      }
    }

    // X / C == Target

    if (Val.isNegative() || Target.isNegative()) {
      if (Val != 0 && Target.srem(Val) == 0) {
        for_no_nop(X, Val * Target) {
          Results.push_back(Builder(X, IC).SDiv(I)());
        }
      }
    } else {
      if (Val != 0 && Target.urem(Val) == 0) {
        for_no_nop(X, Val * Target) {
          Results.push_back(Builder(X, IC).UDiv(I)());
        }
      }
    }

    // Shifts?

    // Unary operators (no recursion required)
    if (Target == Val.logBase2()) {
      Results.push_back(Builder(I, IC).LogB()());
    }

    if (Target == Val.reverseBits()) {
      Results.push_back(Builder(I, IC).BitReverse()());
    }
    // TODO Add others

    // bit flip
    llvm::APInt D = Val;
    D.flipAllBits();
    if (Target == D) {
      Results.push_back(Builder(I, IC).Xor(llvm::APInt::getAllOnesValue(I->Width))());
    }

    if (Target == D + 1) {
      Results.push_back(Builder(I, IC).Xor(llvm::APInt::getAllOnesValue(I->Width)).Add(1)());
    }

    // neg
    D = Val;
    D.negate();
    if (Target == D && D != Val) {
      Results.push_back(Builder(IC, llvm::APInt::getAllOnesValue(I->Width)).Sub(I)());
    }

    for (const auto &[I2, Val2] : ConstMap) {
      if (I == I2 || I->Width != I2->Width || I2 == ParentConst) {
        continue;
      }
      if ((Val & Val2) == Target && !Val.isAllOnesValue() && !Val2.isAllOnesValue()) {
        Results.push_back(Builder(I, IC).And(I2)());
      }
      if ((Val | Val2) == Target && Val != 0 && Val2 != 0) {
        Results.push_back(Builder(I, IC).Or(I2)());
      }
      if ((Val ^ Val2) == Target && Val != Target && Val2 != Target) {
        Results.push_back(Builder(I, IC).Xor(I2)());
      }
    }
  }

  return Results;
}

std::map<Inst *, size_t> CountUses(Inst *I) {
  std::vector<Inst *> Stack{I};
  std::set<Inst *> Visited;
  std::map<Inst *, size_t> Count;
  while (!Stack.empty()) {
    auto *I = Stack.back();
    Stack.pop_back();
    if (Visited.count(I)) {
      continue;
    }
    Visited.insert(I);
    for (auto *U : I->Ops) {
      if (U->K == Inst::Var) {
        Count[U]++;
      }
      Stack.push_back(U);
    }
  }
  return Count;
}

// // Filter candidates to rule out NOPs as much as possible
// std::vector<Inst *> FilterCand(std::vector<Inst *> Cands,
// const std::vector<std::pair<Inst *, llvm::APInt>> &ConstMap) {
//   return Cands;
//   std::vector<Inst *> Results;
//   for (auto &&C : Cands) {
//     std::map<Inst *, size_t> VarCount = CountUses(C);

//     C->Print();
//     for (auto &[I, Count] : VarCount) {
//       llvm::errs() << I->Name << " " << Count << "\t";
//     }
//     llvm::errs() << "\n\n";


//     bool hasDupe = false;
//     for (auto &[_, Count] : VarCount) {
//       if (Count > 4) {
//         hasDupe = true;
//         break;
//       }
//     }
//     if (hasDupe) {
//       continue;
//     }

//     Results.push_back(C);
//   }
//   return Results;
// }

std::vector<std::vector<Inst *>>
InferSpecialConstExprsAllSym(std::vector<Inst *> RHS,
const std::vector<std::pair<Inst *, llvm::APInt>> &ConstMap,
                             InstContext &IC, int depth = 3) {
  std::vector<std::vector<Inst *>> Results;
  for (auto R : RHS) {
    auto Cands = IOSynthesize(R->Val, ConstMap, IC, depth, false);
    std::set<Inst *> Temp;
    for (auto C : Cands) {
      Temp.insert(C);
    }
    std::vector<Inst *> DedupedCands;
    for (auto C : Temp) {
      DedupedCands.push_back(C);
    }
    Results.push_back(DedupedCands);
    std::sort(Results.back().begin(), Results.back().end(),
              [](Inst *A, Inst *B) { return instCount(A) < instCount(B);});
  }
  return Results;
}

std::pair<ConstMapT, ParsedReplacement>
AugmentForSymDB(ParsedReplacement Original, InstContext &IC) {
  auto Input = Clone(Original, IC);
  std::vector<std::pair<Inst *, llvm::APInt>> ConstMap;
  if (Input.Mapping.LHS->DemandedBits.getBitWidth() == Input.Mapping.LHS->Width &&
    !Input.Mapping.LHS->DemandedBits.isAllOnesValue()) {
    auto DB = Input.Mapping.LHS->DemandedBits;
    auto SymDFVar = IC.createVar(DB.getBitWidth(), "symDF_DB");
    // SymDFVar->Name = "symDF_DB";

    SymDFVar->KnownOnes = llvm::APInt(DB.getBitWidth(), 0);
    SymDFVar->KnownZeros = llvm::APInt(DB.getBitWidth(), 0);
    // SymDFVar->Val = DB;

    Input.Mapping.LHS->DemandedBits.setAllBits();
    Input.Mapping.RHS->DemandedBits.setAllBits();

    auto W = Input.Mapping.LHS->Width;

    Input.Mapping.LHS = IC.getInst(Inst::DemandedMask, W, {Input.Mapping.LHS, SymDFVar});
    Input.Mapping.RHS = IC.getInst(Inst::DemandedMask, W, {Input.Mapping.RHS, SymDFVar});

    ConstMap.push_back({SymDFVar, DB});
  }
  return {ConstMap, Input};
}

std::pair<ConstMapT, ParsedReplacement>
AugmentForSymKB(ParsedReplacement Original, InstContext &IC) {
  auto Input = Clone(Original, IC);
  ConstMapT ConstMap;
  std::vector<Inst *> Inputs;
  findVars(Input.Mapping.LHS, Inputs);

  for (auto &&I : Inputs) {
    auto Width = I->Width;
    if (I->KnownZeros.getBitWidth() == I->Width &&
        I->KnownOnes.getBitWidth() == I->Width &&
        !(I->KnownZeros == 0 && I->KnownOnes == 0)) {
      if (I->KnownZeros != 0) {
        Inst *Zeros = IC.createVar(Width, "symDF_K0");

        // Inst *AllOnes = IC.getConst(llvm::APInt::getAllOnesValue(Width));
        // Inst *NotZeros = IC.getInst(Inst::Xor, Width,
        //                         {Zeros, AllOnes});
        // Inst *VarNotZero = IC.getInst(Inst::Or, Width, {I, NotZeros});
        // Inst *ZeroBits = IC.getInst(Inst::Eq, 1, {VarNotZero, NotZeros});
        Inst *ZeroBits = IC.getInst(Inst::KnownZerosP, 1, {I, Zeros});
        Input.PCs.push_back({ZeroBits, IC.getConst(llvm::APInt(1, 1))});
        ConstMap.push_back({Zeros, I->KnownZeros});
        I->KnownZeros = llvm::APInt(I->Width, 0);
      }

      if (I->KnownOnes != 0) {
        Inst *Ones = IC.createVar(Width, "symDF_K1");
        // Inst *VarAndOnes = IC.getInst(Inst::And, Width, {I, Ones});
        // Inst *OneBits = IC.getInst(Inst::Eq, 1, {VarAndOnes, Ones});
        Inst *OneBits = IC.getInst(Inst::KnownOnesP, 1, {I, Ones});
        Input.PCs.push_back({OneBits, IC.getConst(llvm::APInt(1, 1))});
        ConstMap.push_back({Ones, I->KnownOnes});
        I->KnownOnes = llvm::APInt(I->Width, 0);
      }
    }
  }
  return {ConstMap, Input};
}

// // Harvest synthesis sketch from LHS
// std::function<Inst *(std::vector<Inst *>)> GetSketch(Inst *LHS) {
//   std::vector<Inst *> Inputs;
//   findVars(LHS, Inputs);


// }

std::vector<std::vector<Inst *>>
InferSpecialConstExprsWithConcretes(std::vector<Inst *> RHS,
const std::vector<std::pair<Inst *, llvm::APInt>> &ConstMap,
                             InstContext &IC, int depth = 3) {
  std::vector<std::vector<Inst *>> Results;
  for (auto R : RHS) {
    auto Cands = IOSynthesize(R->Val, ConstMap, IC, depth, true);
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
                                           std::set<Inst *> AtomicComps, InstContext &IC,
                                           const std::vector<std::pair<Inst *, llvm::APInt>> &ConstMap,
                                           size_t NumInsts = 1) {
    std::vector<std::vector<Inst *>> Candidates;

    std::vector<Inst *> Components;
    for (auto &&C : AtomicComps) {
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
      std::vector<Inst *> CandsForTarget;
      EnumerativeSynthesis ES;
      auto Guesses = ES.generateExprs(IC, NumInsts, Components,
                                      Target->Width);
      for (auto &&Guess : Guesses) {
        std::set<Inst *> ConstSet;
        souper::getConstants(Guess, ConstSet);
        if (!ConstSet.empty()) {
          if (SymbolizeConstSynthesis) {
            CandsForTarget.push_back(Guess);
          }
        } else {
          CandsForTarget.push_back(Guess);
        }
      }
      // Filter by value
      Candidates.push_back(FilterExprsByValue(CandsForTarget, Target->Val, ConstMap));
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

// TODO: memoize
bool hasMultiArgumentPhi(Inst *I) {
  if (I->K == Inst::Phi) {
    return I->Ops.size() > 1;
  }
  for (auto Op : I->Ops) {
    if (hasMultiArgumentPhi(Op)) {
      return true;
    }
  }
  return false;
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

ParsedReplacement DeAugment(InstContext &IC,
                            Solver *S, ParsedReplacement Augmented) {
  auto Result = ReduceBasic(IC, S, Augmented);
  Inst *SymDBVar = nullptr;
  if (Result.Mapping.LHS->K == Inst::DemandedMask) {
    SymDBVar = Result.Mapping.LHS->Ops[1];
  }

  if (!SymDBVar) {
    return Result;
  }

  auto LHSUses = CountUses(Result.Mapping.LHS);
  auto RHSUses = CountUses(Result.Mapping.RHS);

  if (LHSUses[SymDBVar] == 1 && RHSUses[SymDBVar] == 1) {
    // We can remove the SymDBVar
    Result.Mapping.LHS = Result.Mapping.LHS->Ops[0];
    Result.Mapping.RHS = Result.Mapping.RHS->Ops[0];
    return Result;
  } else {
    return Result;
  }
}

// Assuming the input has leaves pruned and preconditions weakened
ParsedReplacement SuccessiveSymbolize(InstContext &IC,
                            Solver *S, ParsedReplacement Input, bool &Changed,
                            std::vector<std::pair<Inst *, llvm::APInt>> ConstMap = {}) {

  // Print first successful result and exit, no result sorting.
  // Prelude
  bool Nested = !ConstMap.empty();
  auto Original = Input;
  if (!NoWidth && !Nested && !hasMultiArgumentPhi(Input.Mapping.LHS)) {
    ShrinkWrap Shrink(IC, S, Input, 8);
    auto Smol = Shrink();
    if (Smol) {
      if (DebugLevel > 2) {
        llvm::errs() << "Shrinked: \n";
        InfixPrinter P(Smol.value());
        P(llvm::errs());
        Smol->print(llvm::errs(), true);
        llvm::errs() << "\n";
        if (DebugLevel > 4) {
          Smol.value().print(llvm::errs(), true);
        }
      }
      Input = Smol.value();

      // Input.print(llvm::errs(), true);

    } else {
      if (DebugLevel > 2) {
        llvm::errs() << "Shrinking failed\n";
      }
    }
  }

  auto Fresh = Input;
  size_t ticks = std::clock();
  auto Refresh = [&] (auto Msg) {
    // Input = Clone(Fresh, IC);
    Input = Fresh;
    if (DebugLevel > 2) {
      auto now = std::clock();
      llvm::errs() << "POST " << Msg << " - " << (now - ticks)*1000/CLOCKS_PER_SEC << " ms\n";
      ticks = now;
    }
    Changed = true;
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
    SymConstMap[I] = IC.createVar(I->Width, Name);

    // llvm::errs() << "HERE : " << Name << '\t' << SymConstMap[I]->Name << "\n";

    InstCache[I] = SymConstMap[I];
    SymCS[SymConstMap[I]] = I->Val;
  }
  for (auto I : RHSConsts) {
    if (SymConstMap.find(I) != SymConstMap.end()) {
      continue;
    }
    auto Name = "symconst_" + std::to_string(i++);
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

    // llvm::errs() << "Common Const: " << C->Val << "\t" << SymConstMap[C]->Name << "\n";

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

//    Clone = DFPreconditionsAndVerifyGreedy(Result, IC, S, SymCS);
//    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
//      return Clone;
//    }

  }

  Refresh("Direct Symbolize for common consts");

  // Step 1.5 : Direct symbolize, simple rel constraints on LHS

  for (auto &&C : LHSConsts) {
    ConstMap.push_back({SymConstMap[C], C->Val});
  }
  auto CounterExamples = GetMultipleCEX(Result, IC, S, 3);
  if (Nested) {
    CounterExamples = {};
    // FIXME : Figure out how to get CEX for symbolic dataflow
  }
  auto Relations = InferPotentialRelations(ConstMap, IC, Input, CounterExamples, Nested);

  std::map<Inst *, Inst *> JustLHSSymConstMap;

  for (auto &&C : LHSConsts) {
    JustLHSSymConstMap[C] = SymConstMap[C];
  }

  auto Copy = Replace(Input, IC, JustLHSSymConstMap);
  // for (auto &&R : Relations) {
  //   Copy.PCs.push_back({R, IC.getConst(llvm::APInt(1, 1))});
  //   // Copy.print(llvm::errs(), true);
  //   auto Clone = Verify(Copy, IC, S);
  //   if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
  //     return Clone;
  //   }
  //   Copy.PCs.pop_back();
  // }

  if (auto RelV = VerifyWithRels(IC, S, Copy, Relations)) {
    return RelV.value();
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

  auto ConstantLimits = InferConstantLimits(ConstMap, IC, Input, CounterExamples);

  // Step 3 : Special RHS constant exprs, no constants

  if (!RHSFresh.empty()) {

  std::vector<std::vector<Inst *>> UnitaryCandidates =
    InferSpecialConstExprsAllSym(RHSFresh, ConstMap, IC, /*depth*/0);

  if (!UnitaryCandidates.empty()) {
    // if (Nested && DebugLevel > 4) {
    //   llvm::errs() << "Rels " << Relations.size() << "\n";
    //   llvm::errs() << "Unitary candidates: " << UnitaryCandidates[0].size() << "\n";
    //   llvm::errs() << "FOO: " << UnitaryCandidates[0][0]->Name << "\n";
    // }

    auto Clone = FirstValidCombination(Input, RHSFresh, UnitaryCandidates,
                                  InstCache, IC, S, SymCS, true, false, false, Relations);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Unitary cands, rel constraints");
  }

  std::vector<std::vector<Inst *>> SimpleCandidates =
    InferSpecialConstExprsAllSym(RHSFresh, ConstMap, IC, /*depth=*/ 2);

  if (!SimpleCandidates.empty()) {
    if (DebugLevel > 4) {
      llvm::errs() << "InferSpecialConstExprsAllSym candidates: " << SimpleCandidates[0].size() << " x " << ConstantLimits.size() << "\n";
    }
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidates,
                                       InstCache, IC, S, SymCS,
                                       true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidates,
                                  InstCache, IC, S, SymCS, true, false, false, ConstantLimits);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

  }
  Refresh("Special expressions, no constants");

  // Step 4 : Enumerated expressions

  std::set<Inst *> Components;
  for (auto C : ConstMap) {
    Components.insert(C.first);
  }

  auto EnumeratedCandidates = Enumerate(RHSFresh, Components, IC, ConstMap);
  //   if (DebugLevel > 4) {
  //   llvm::errs() << "RHSFresh: " << RHSFresh.size() << "\n";
  //   llvm::errs() << "Components: " << Components.size() << "\n";
  //   llvm::errs() << "EnumeratedCandidates: " << EnumeratedCandidates[0].size() << "\n";
  // }

  if (!EnumeratedCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                  InstCache, IC, S, SymCS, true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Enumerated cands, no constraints");
  }


  // Step 4.75 : Enumerate 2 instructions when single RHS Constant.
  std::vector<std::vector<Inst *>> EnumeratedCandidatesTwoInsts;
  if (RHSFresh.size() == 1) {
    EnumeratedCandidatesTwoInsts = Enumerate(RHSFresh, Components, IC, ConstMap, 2);

    // llvm::errs() << "Guesses: " << EnumeratedCandidatesTwoInsts[0].size() << "\n";

    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidatesTwoInsts,
                                  InstCache, IC, S, SymCS, true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
  }
  Refresh("Enumerated 2 insts for single RHS const cases");

  if (!EnumeratedCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                  InstCache, IC, S, SymCS, false, true, true);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    // Enumerated Expressions with some relational constraints
    if (ConstMap.size() == 2) {
      // llvm::errs() << "Relations: " << Relations.size() << "\n";
      // llvm::errs() << "Guesses: " << EnumeratedCandidates[0].size() << "\n";

      auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                          InstCache, IC, S, SymCS, true, false, false, Relations);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
    }
    Refresh("Relational constraints for enumerated cands.");

  }
  Refresh("Enumerated exprs with constraints");

  if (RHSFresh.size() == 1 && !Nested) {
    // Enumerated Expressions with some relational constraints
    if (ConstMap.size() == 2) {
      // llvm::errs() << "Enum2 : " << EnumeratedCandidatesTwoInsts.back().size()
                  //  << "\tRels: " << Relations.size() << "\n";
      auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidatesTwoInsts,
                                          InstCache, IC, S, SymCS, true, false, false, Relations);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        return Clone;
      }
    }
  }
  Refresh("Enumerated 2 insts exprs with relations");

  // Step 4.8 : Special RHS constant exprs, with constants

  std::vector<std::vector<Inst *>> SimpleCandidatesWithConsts =
    InferSpecialConstExprsWithConcretes(RHSFresh, ConstMap, IC, /*depth=*/ 2);

  if (!SimpleCandidatesWithConsts.empty() && !Nested) {
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                        InstCache, IC, S, SymCS,
                                        true, false, false);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
  }

  Refresh("Special expressions, with constants");

  // Enumerated exprs with constraints

  if (!EnumeratedCandidates.empty() && !Nested) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                        InstCache, IC, S, SymCS, true, true, true, Relations);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
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

    Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidates,
                                        InstCache, IC, S, SymCS, true, false, false, Relations);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Simple cands with constraints and relations");
  }

  // Step 5.5 : Simple exprs with constraints

  if (!SimpleCandidatesWithConsts.empty() && !Nested) {
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                       InstCache, IC, S, SymCS, false, true, true);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Simple cands+consts with constraints");

    Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                        InstCache, IC, S, SymCS, true, true, true, Relations);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }

    Refresh("Simple cands+consts with constraints and relations");
  }

  // {
  //   if (!RHSFresh.empty()) {
  //     std::vector<std::vector<Inst *>> SimpleCandidatesMoreInsts =
  //       InferSpecialConstExprsAllSym(RHSFresh, ConstMap, IC, /*depth =*/ 5);

  //     if (!SimpleCandidates.empty()) {
  //       auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesMoreInsts,
  //                                         InstCache, IC, S, SymCS,
  //                                         true, false, false);
  //       if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
  //         return Clone;
  //       }
  //     }

  //     Refresh("Special expressions, no constants");
  //   }
  // }

  if (!EnumeratedCandidates.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, EnumeratedCandidates,
                                        InstCache, IC, S, SymCS, true, true, false, ConstantLimits);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Enumerated expressions+consts and constant limits");
  }

  if (!SimpleCandidatesWithConsts.empty()) {
    auto Clone = FirstValidCombination(Input, RHSFresh, SimpleCandidatesWithConsts,
                                        InstCache, IC, S, SymCS, true, false, false, ConstantLimits);
    if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
      return Clone;
    }
    Refresh("Simple expressions+consts and constant limits");
  }

  }

  {
    auto Copy = Replace(Input, IC, JustLHSSymConstMap);
    if (auto VRel = VerifyWithRels(IC, S, Copy, ConstantLimits)) {
      return VRel.value();
    }
    Refresh("Constant limit constraints on LHS");
  }

  if (SymbolicDF) {
    Refresh("PUSH SYMDF_KB_DB");
    auto [CM, Aug] = AugmentForSymKBDB(Input, IC);
    // auto [CM2, Aug2] = AugmentForSymKB(Aug1, IC);
    if (!CM.empty()) {
      bool SymDFChanged = false;

      auto Clone = Verify(Aug, IC, S);
      if (Clone.Mapping.LHS && Clone.Mapping.RHS) {
        // Symbolic db+kb can be unconstrained
        // very unlikely? test if needed
        return Clone;
      }

      auto Generalized = SuccessiveSymbolize(IC, S, Aug, SymDFChanged, CM);
      if (SymDFChanged) {
        return Generalized;
      }
    }
    Refresh("POP SYMDF_KB_DB");

  }

  Refresh("END");
  Changed = false;
  return Input;
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

std::pair<ParsedReplacement, bool>
InstantiateWidthChecks(InstContext &IC,
  Solver *S, ParsedReplacement Input) {
  if (!NoWidth && !hasMultiArgumentPhi(Input.Mapping.LHS)) {
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
      return {Input, true};
    }

    auto &&ValidTypings = Alive.getValidTypings();

    if (ValidTypings.empty()) {
      // Something went wrong, generalized opt is not valid at any width.
      if (DebugLevel > 4) {
        llvm::errs() << "WIDTH: Generalized opt is not valid for any width.\n";
      }
      Input.Mapping.LHS = nullptr;
      Input.Mapping.RHS = nullptr;
      return {Input, false};
    }

  // Abstract width to a range or relational precondition
  // TODO: Abstraction
  }

  // If abstraction fails, insert checks for existing widths.
  std::vector<Inst *> Inputs;
  findVars(Input.Mapping.LHS, Inputs);
  for (auto &&I : Inputs) {
    Input.PCs.push_back(GetEqWidthConstraint(I, I->Width, IC));
  }
  return {Input, false};
}

template<typename Stream>
Stream &operator<<(Stream &S, InfixPrinter IP) {
  IP(S);
  return S;
}

void tagConstExprs(Inst *I, std::set<Inst *> &Set) {
  if (I->K == Inst::Const || (I->K == Inst::Var && I->Name.starts_with("sym"))) {
    Set.insert(I);
  } else {
    for (auto Op : I->Ops) {
      tagConstExprs(Op, Set);
    }
  }

  if (I->Ops.size() > 0) {
    bool foundNonConst = false;
    for (auto Op : I->Ops) {
      if (Set.find(Op) == Set.end()) {
        foundNonConst = true;
        break;
      }
    }
    if (!foundNonConst) {
      Set.insert(I);
    }
  }
}

size_t constAwareCost(Inst *I) {
  std::set<Inst *> ConstExprs;
  tagConstExprs(I, ConstExprs);
  return souper::cost(I, false, ConstExprs);
}

int profit(const ParsedReplacement &P) {
  return constAwareCost(P.Mapping.LHS) - constAwareCost(P.Mapping.RHS);
}

void PrintInputAndResult(ParsedReplacement Input, ParsedReplacement Result) {
  ReplacementContext RC;
  Result.printLHS(llvm::outs(), RC, true);
  Result.printRHS(llvm::outs(), RC, true);
  llvm::outs() << "\n";

  if (DebugLevel > 1) {
      llvm::errs() << "IR Input: \n";
    ReplacementContext RC;
    Input.printLHS(llvm::outs(), RC, true);
    Input.printRHS(llvm::outs(), RC, true);
    llvm::outs() << "\n";
    llvm::errs() << "\n\tInput (profit=" << profit(Input) <<  "):\n\n"
                  << InfixPrinter(Input)
                  << "\n\tGeneralized (profit=" << profit(Result) << "):\n\n"
                  << InfixPrinter(Result, NoWidth) << "\n";
    Result.print(llvm::errs(), true);
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
    if (Input.Mapping.LHS == Input.Mapping.RHS) {
      if (DebugLevel > 2) llvm::errs() << "Input == Output\n";
      continue;
    } else if (profit(Input) < 0) {
      if (DebugLevel > 2) llvm::errs() << "Not an optimization\n";
      continue;
    }
    if (Basic) {
      ParsedReplacement Result = ReduceBasic(IC, S.get(), Input);
      if (!JustReduce) {

        bool Changed = false;
        size_t MaxTries = 1; // Increase this if we ever run with 10/100x timeout.
        do {
          if (!OnlyWidth) {
            if (Changed) {
              Result = ReduceBasic(IC, S.get(), Result);
            }
            Result = SuccessiveSymbolize(IC, S.get(), Result, Changed);
          }
          bool Indep = false;
          if (!NoWidth) {
            std::tie(Result, Indep) = InstantiateWidthChecks(IC, S.get(), Result);
          }
//          Result.print(llvm::errs(), true);
          if (!Result.Mapping.LHS && !NoWidth) {
            Result = Input;
            MaxTries++;
            NoWidth = true;
            continue; // Retry with no width checks
          }

          if (!Indep && Result.Mapping.LHS && !NoWidth) {
            MaxTries++;
            NoWidth = true;
            PrintInputAndResult(Input, Result);
            Result = Input;
            continue; // Retry with no width checks
          }
          Result = DeAugment(IC, S.get(), Result);
        } while (--MaxTries && Changed);
      }
      if (Result.Mapping.LHS && Result.Mapping.RHS) {
        PrintInputAndResult(Input, Result);
      }
    }
  }
  return 0;
}
