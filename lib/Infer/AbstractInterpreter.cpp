#include "souper/Infer/Interpreter.h"

namespace souper {
  bool isReservedConst(Inst *I) {
    return I->K == Inst::ReservedConst ||
          (I->K == Inst::Var &&
          (I->Name.find(ReservedConstPrefix) != std::string::npos));
  }

  bool isReservedInst(Inst *I) {
    return I->K == Inst::ReservedInst ||
          (I->K == Inst::Var &&
          (I->Name.find(ReservedInstPrefix) != std::string::npos));
  }

  bool hasReservedHelper(Inst *I, std::set<Inst *> &Visited,
                         bool ConsiderConsts,
                         bool ConsiderHoles) {
    if (ConsiderConsts && isReservedConst(I)) {
      return true;
    }
    if (ConsiderHoles && isReservedInst(I)) {
      return true;
    }
    if (Visited.insert(I).second)
      for (auto Op : I->Ops)
        if (hasReservedHelper(Op, Visited, ConsiderConsts, ConsiderHoles))
          return true;
    return false;
  }

  bool isConcrete(Inst *I, bool ConsiderConsts, bool ConsiderHoles) {
    std::set<Inst *> Visited;
    return !hasReservedHelper(I, Visited, ConsiderConsts, ConsiderHoles);
  }

  EvalValue getValue(Inst *I, ValueCache &C, bool PartialEval) {
    if (I->K == Inst::Const)
      return {I->Val};
    if (C.find(I) != C.end()) {
      return C[I];
    } else {
      if (PartialEval && isConcrete(I)) {
        return evaluateInst(I, C);
      }
    }
    // unimplemented
    return EvalValue();
  }

#define KB0 findKnownBits(I->Ops[0], C, PartialEval)
#define KB1 findKnownBits(I->Ops[1], C, PartialEval)
#define VAL(INST) getValue(INST, C, PartialEval)

  llvm::KnownBits findKnownBits(Inst *I, ValueCache &C, bool PartialEval) {
    llvm::KnownBits Result(I->Width);
    if (PartialEval && isConcrete(I)) {
      auto RootVal = evaluateInst(I, C);
      if (RootVal.hasValue()) {
        Result.One = RootVal.getValue();
        Result.Zero = ~RootVal.getValue();
        return Result;
      }
    }
    switch(I->K) {
    case Inst::Const:
    case Inst::Var : {
      EvalValue V = VAL(I);
      if (V.hasValue()) {
        Result.One = V.getValue();
        Result.Zero = ~V.getValue();
        return Result;
      } else {
        return Result;
      }
    }
//   case Phi:
//     return "phi";
    case Inst::AddNUW :
    case Inst::AddNW :
    case Inst::Add: {
      const auto &Op0KB = KB0;
      const auto &Op1KB = KB1;
      return llvm::KnownBits::computeForAddSub(/*Add=*/true, /*NSW=*/false,
                                               Op0KB, Op1KB);
    }
    case Inst::AddNSW: {
      const auto &Op0KB = KB0;
      const auto &Op1KB = KB1;
      return llvm::KnownBits::computeForAddSub(/*Add=*/true, /*NSW=*/true,
                                               Op0KB, Op1KB);
    }
    case Inst::SubNUW :
    case Inst::SubNW :
    case Inst::Sub: {
      const auto &Op0KB = KB0;
      const auto &Op1KB = KB1;
      return llvm::KnownBits::computeForAddSub(/*Add=*/false, /*NSW=*/false,
                                               Op0KB, Op1KB);
    }
    case Inst::SubNSW: {
      const auto &Op0KB = KB0;
      const auto &Op1KB = KB1;
      return llvm::KnownBits::computeForAddSub(/*Add=*/false, /*NSW=*/true,
                                               Op0KB, Op1KB);
    }
//   case Mul:
//     return "mul";
//   case MulNSW:
//     return "mulnsw";
//   case MulNUW:
//     return "mulnuw";
//   case MulNW:
//     return "mulnw";
    case Inst::UDiv: {
      unsigned LeadZ = KB0.countMinLeadingZeros();
      unsigned RHSMaxLeadingZeros = KB1.countMaxLeadingZeros();
      if (RHSMaxLeadingZeros != I->Width)
        LeadZ = std::min(I->Width, LeadZ + I->Width - RHSMaxLeadingZeros - 1);
      Result.Zero.setHighBits(LeadZ);
      return Result;
    }
//   case SDiv:
//     return "sdiv";
//   case UDivExact:
//     return "udivexact";
//   case SDivExact:
//     return "sdivexact";
    case Inst::URem: {
      auto Op1V = VAL(I->Ops[1]);
      if (Op1V.hasValue()) {
        auto RA = Op1V.getValue();
        if (RA.isPowerOf2()) {
          auto LowBits = (RA - 1);
          Result = KB0;
          Result.Zero |= ~LowBits;
          Result.One &= LowBits;
          return Result;
        }
      }

      unsigned Leaders =
        std::max(KB0.countMinLeadingZeros(), KB1.countMinLeadingZeros());
      Result.resetAll();
      Result.Zero.setHighBits(Leaders);
      return Result;
    }
//   case SRem:
//     return "srem";
    case Inst::And : {
      auto Op0KB = KB0;
      auto Op1KB = KB1;

      Op0KB.One &= Op1KB.One;
      Op0KB.Zero |= Op1KB.Zero;
      return Op0KB;
    }
    case Inst::Or : {
      auto Op0KB = KB0;
      auto Op1KB = KB1;

      Op0KB.One |= Op1KB.One;
      Op0KB.Zero &= Op1KB.Zero;
      return Op0KB;
    }
    case Inst::Xor : {
      auto Op0KB = KB0;
      auto Op1KB = KB1;
      llvm::APInt KnownZeroOut =
        (Op0KB.Zero & Op1KB.Zero) | (Op0KB.One & Op1KB.One);
      Op0KB.One = (Op0KB.Zero & Op1KB.One) | (Op0KB.One & Op1KB.Zero);
      Op0KB.Zero = std::move(KnownZeroOut);
      // ^ logic copied from LLVM ValueTracking.cpp
      return Op0KB;
    }
    case Inst::ShlNSW :
    case Inst::ShlNUW :
    case Inst::ShlNW : // TODO: Rethink if these make sense
    case Inst::Shl : {
      auto Op0KB = KB0;
      auto Op1V = VAL(I->Ops[1]);

      if (Op1V.hasValue()) {
        auto Val = Op1V.getValue().getLimitedValue();
        if (Val < 0 || Val >= I->Width) {
          return Result;
        }
        Op0KB.One <<= Val;
        Op0KB.Zero <<= Val;
        Op0KB.Zero.setLowBits(Val);
        // setLowBits takes an unsigned int, so getLimitedValue is harmless
        return Op0KB;
      } else if (isReservedConst(I->Ops[1])) {
        Result.Zero.setLowBits(1);
        return Result;
      } else {
        return Result;
      }
    }
//   case ShlNSW:
//     return "shlnsw";
//   case ShlNUW:
//     return "shlnuw";
//   case ShlNW:
//     return "shlnw";
    case Inst::LShr : {
      auto Op0KB = KB0;
      auto Op1V = VAL(I->Ops[1]);
      if (Op1V.hasValue()) {
        auto Val = Op1V.getValue().getLimitedValue();
        if (Val < 0 || Val >= I->Width) {
          return Result;
        }
        Op0KB.One <<= Val;
        Op0KB.Zero <<= Val;
        Op0KB.Zero.setHighBits(Val);
        // setHighBits takes an unsigned int, so getLimitedValue is harmless
        return Op0KB;
      } else {
        return Result;
      }
    }
//   case LShrExact:
//     return "lshrexact";
//   case AShr:
//     return "ashr";
//   case AShrExact:
//     return "ashrexact";
//   case Select:
//     return "select";
    case Inst::ZExt: {
      return KB0.zext(I->Width);
    }
    case Inst::SExt: {
      return KB0.sext(I->Width);
    }
    case Inst::Trunc: {
      return KB0.trunc(I->Width);
    }

    case Inst::Eq: {
      Inst *Constant = nullptr;
      llvm::KnownBits Other;
      if (isReservedConst(I->Ops[0])) {
        Constant = I->Ops[0];
        Other = KB1;
      } else if (isReservedConst(I->Ops[1])) {
        Constant = I->Ops[1];
        Other = KB0;
      } else {
        return Result;
      }
      if ((Other.Zero & 1) != 0) {
        Result.Zero.setBit(0);
        return Result;
      } else {
        return Result;
      }
    }
    case Inst::Ne: {
      auto Op0KB = KB0;
      auto Op1KB = KB1;
      llvm::APInt Cond = (Op0KB.Zero & ~Op1KB.One) | (Op0KB.One & ~Op1KB.Zero);
      bool Conflict = Cond != 0;
      if (Conflict) {
        Result.One.setBit(0);
        return Result;
      }
      return Result;
    }
//   case Ult:
//     return "ult";
//   case Slt:
//     return "slt";
//   case Ule:
//     return "ule";
//   case Sle:
//     return "sle";
//   case CtPop:
//     return "ctpop";
    case Inst::BSwap: {
      auto Op0KB = KB0;
      Op0KB.One = Op0KB.One.byteSwap();
      Op0KB.Zero = Op0KB.Zero.byteSwap();
      return Op0KB;
    }
    case Inst::BitReverse: {
      auto Op0KB = KB0;
      Op0KB.One = Op0KB.One.reverseBits();
      Op0KB.Zero = Op0KB.Zero.reverseBits();
      return Op0KB;
    }
//   case Cttz:
//     return "cttz";
//   case Ctlz:
//     return "ctlz";
//   case FShl:
//     return "fshl";
//   case FShr:
//     return "fshr";
//   case ExtractValue:
//     return "extractvalue";
//   case SAddWithOverflow:
//     return "sadd.with.overflow";
//   case UAddWithOverflow:
//     return "uadd.with.overflow";
//   case SSubWithOverflow:
//     return "ssub.with.overflow";
//   case USubWithOverflow:
//     return "usub.with.overflow";
//   case SMulWithOverflow:
//     return "smul.with.overflow";
//   case UMulWithOverflow:
//     return "umul.with.overflow";
//   case ReservedConst:
//     return "reservedconst";
//   case ReservedInst:
//     return "reservedinst";
//   case SAddO:
//   case UAddO:
//   case SSubO:
//   case USubO:
//   case SMulO:
//   case UMulO:
    default :
      return Result;
    }
  }

#undef KB0
#undef KB1

#define CR0 findConstantRange(I->Ops[0], C, PartialEval)
#define CR1 findConstantRange(I->Ops[1], C, PartialEval)
#define CR2 findConstantRange(I->Ops[2], C, PartialEval)

  llvm::ConstantRange findConstantRange(Inst *I,
                                        ValueCache &C, bool PartialEval) {
    llvm::ConstantRange Result(I->Width);

    if (PartialEval && isConcrete(I)) {
      auto RootVal = evaluateInst(I, C);
      if (RootVal.hasValue()) {
        return llvm::ConstantRange(RootVal.getValue());
      }
    }

    switch (I->K) {
    case Inst::Const:
    case Inst::Var : {
      EvalValue V = VAL(I);
      if (V.hasValue()) {
        return llvm::ConstantRange(V.getValue());
      } else {
        if (isReservedConst(I)) {
          return llvm::ConstantRange(llvm::APInt(I->Width, 0)).inverse();
        }
        return Result; // Whole range
      }
    }
    case Inst::Trunc: {
      return CR0.truncate(I->Width);
    }
    case Inst::SExt: {
      return CR0.signExtend(I->Width);
    }
    case Inst::ZExt: {
      return CR0.zeroExtend(I->Width);
    }
    case souper::Inst::AddNUW :
    case souper::Inst::AddNW : // TODO: Rethink if these make sense
    case Inst::Add: {
      return CR0.add(CR1);
    }
    case Inst::AddNSW: {
      auto V1 = VAL(I->Ops[1]);
      if (V1.hasValue()) {
        return CR0.addWithNoSignedWrap(V1.getValue());
      } else {
        return Result; // full range, can we do better?
      }
    }
    case souper::Inst::SubNSW :
    case souper::Inst::SubNUW :
    case souper::Inst::SubNW : // TODO: Rethink if these make sense
    case Inst::Sub: {
      return CR0.sub(CR1);
    }
    case souper::Inst::MulNSW :
    case souper::Inst::MulNUW :
    case souper::Inst::MulNW : // TODO: Rethink if these make sense
    case Inst::Mul: {
      return CR0.multiply(CR1);
    }
    case Inst::And: {
      return CR0.binaryAnd(CR1);
    }
    case Inst::Or: {
      return CR0.binaryOr(CR1);
    }
    case souper::Inst::ShlNSW :
    case souper::Inst::ShlNUW :
    case souper::Inst::ShlNW : // TODO: Rethink if these make sense
    case Inst::Shl: {
      return CR0.shl(CR1);
    }
    case Inst::AShr: {
      return CR0.ashr(CR1);
    }
    case Inst::LShr: {
      return CR0.lshr(CR1);
    }
    case Inst::UDiv: {
      return CR0.udiv(CR1);
    }
    case Inst::Ctlz:
    case Inst::Cttz:
    case Inst::CtPop: {
      return llvm::ConstantRange(llvm::APInt(I->Width, 0),
                                 llvm::APInt(I->Width, I->Ops[0]->Width + 1));
    }
    case Inst::Select: {
      return CR1.unionWith(CR2);
    }
      //     case Inst::SDiv: {
      //       auto R0 = FindConstantRange(I->Ops[0], C);
      //       auto R1 = FindConstantRange(I->Ops[1], C);
      //       return R0.sdiv(R1); // unimplemented
      //     }
      // TODO: Xor pattern for not, truncs and extends, etc
    default:
      return Result;
    }
  }
#undef CR0
#undef CR1
#undef CR2
#undef VAL
}
