#include "souper/Infer/Interpreter.h"

namespace souper {

  bool hasReservedHelper(Inst *I, std::set<Inst *> &Visited,
                         bool ConsiderConsts,
                         bool ConsiderHoles) {
    if (ConsiderConsts && I->K == Inst::ReservedConst)
      return true;
    if (ConsiderHoles && I->K == Inst::ReservedInst)
      return true;
    if (ConsiderConsts && I->K == Inst::Var
        && (I->Name.find(ReservedConstPrefix) != std::string::npos)) {
      return true;
    }
    if (ConsiderHoles && I->K == Inst::Var
        && (I->Name.find(ReservedInstPrefix) != std::string::npos)) {
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

#define ARG0 Args[0].getValue()
#define ARG1 Args[1].getValue()
#define ARG2 Args[2].getValue()

  EvalValue evaluateSingleInst(Inst *Inst, std::vector<EvalValue> &Args) {

    // UB propagates unconditionally
    for (auto &A : Args)
      if (A.K == EvalValue::ValueKind::UB)
        return EvalValue::ub();

    // phi and select only take poison from their chosen input
    if (Inst->K != Inst::Phi && Inst->K != Inst::Select) {
      for (auto &A : Args)
        if (A.K == EvalValue::ValueKind::Poison)
          return EvalValue::poison();
    }

    for (auto &A : Args)
      if (A.K == EvalValue::ValueKind::Undef)
        llvm::report_fatal_error("undef not supported by interpreter");

    switch (Inst->K) {
    case Inst::Const:
      return {Inst->Val};

    case Inst::UntypedConst:
      return {Inst->Val};

    case Inst::Var:
      llvm::report_fatal_error("Interpreter can't find an input value, exiting");

    case Inst::Phi:
      // FIXME should return all values not just one of them
      return ARG0;

    case Inst::Add:
      return {ARG0 + ARG1};

    case Inst::AddNSW:{
      bool Ov;
      auto Res = ARG0.sadd_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      else
        return Res;
    }

    case Inst::AddNUW:{
      bool Ov;
      auto Res = ARG0.uadd_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      else
        return Res;
    }

    case Inst::AddNW:{
      bool Ov1, Ov2;
      auto Res1 = ARG0.sadd_ov(ARG1, Ov1);
      auto Res2 = ARG0.uadd_ov(ARG1, Ov2);
      if (Ov1 || Ov2)
        return EvalValue::poison();
      else
        return Res1;
    }

    case Inst::Sub:
      return {ARG0 - ARG1};

    case Inst::SubNSW:{
      bool Ov;
      auto Res = ARG0.ssub_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      else
        return Res;
    }

    case Inst::SubNUW:{
      bool Ov;
      auto Res = ARG0.usub_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      else
        return Res;
    }

    case Inst::SubNW:{
      bool Ov1, Ov2;
      auto Res1 = ARG0.ssub_ov(ARG1, Ov1);
      auto Res2 = ARG0.usub_ov(ARG1, Ov2);
      if (Ov1 || Ov2)
        return EvalValue::poison();
      else
        return Res1;
    }

    case Inst::Mul:
      return {ARG0 * ARG1};

    case Inst::MulNSW:{
      bool Ov;
      auto Res = ARG0.smul_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      else
        return Res;
    }

    case Inst::MulNUW:{
      bool Ov;
      auto Res = ARG0.umul_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      else
        return Res;
    }

    case Inst::MulNW:{
      bool Ov1, Ov2;
      auto Res1 = ARG0.smul_ov(ARG1, Ov1);
      auto Res2 = ARG0.umul_ov(ARG1, Ov2);
      if (Ov1 || Ov2)
        return EvalValue::poison();
      else
        return Res1;
    }

    case Inst::UDivExact:
      if (ARG1 == 0)
        return EvalValue::ub();
      if ((ARG0.udiv(ARG1) * ARG1) != ARG0)
        return EvalValue::poison();
      return {ARG0.udiv(ARG1)};

    case Inst::UDiv:
      if (ARG1 == 0)
        return EvalValue::ub();
      return {ARG0.udiv(ARG1)};

    case Inst::SDivExact:
      if (ARG1 == 0 ||
          (ARG0.isMinSignedValue() && ARG1.isAllOnesValue()))
        return EvalValue::ub();
      if ((ARG0.sdiv(ARG1) * ARG1) != ARG0)
        return EvalValue::poison();
      return {ARG0.sdiv(ARG1)};

    case Inst::SDiv:
      if (ARG1 == 0 ||
          (ARG0.isMinSignedValue() && ARG1 == -1))
        return EvalValue::ub();
      return {ARG0.sdiv(ARG1)};

    case Inst::URem:
      if (ARG1 == 0)
        return EvalValue::ub();
      return {ARG0.urem(ARG1)};

    case Inst::SRem:
      if (ARG1 == 0 ||
          (ARG0.isMinSignedValue() && ARG1.isAllOnesValue()))
        return EvalValue::ub();
      return {ARG0.srem(ARG1)};

    case Inst::And:
      return {ARG0 & ARG1};

    case Inst::Or:
      return {ARG0 | ARG1};

    case Inst::Xor:
      return {ARG0 ^ ARG1};

    case Inst::Shl:
      if (ARG1.uge(ARG0.getBitWidth()))
        return EvalValue::poison();
      return {ARG0 << ARG1};

    case Inst::ShlNSW:{
      bool Ov;
      auto Res = ARG0.sshl_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      return Res;
    }

    case Inst::ShlNUW:{
      bool Ov;
      auto Res = ARG0.ushl_ov(ARG1, Ov);
      if (Ov)
        return EvalValue::poison();
      return Res;
    }

    case Inst::ShlNW:{
      bool Ov1, Ov2;
      auto Res1 = ARG0.ushl_ov(ARG1, Ov1);
      auto Res2 = ARG0.sshl_ov(ARG1, Ov2);
      if (Ov1 || Ov2)
        return EvalValue::poison();
      return Res1;
    }

    case Inst::LShr:
      if (ARG1.uge(ARG0.getBitWidth()))
        return EvalValue::poison();
      return {ARG0.lshr(ARG1)};

    case Inst::LShrExact:{
      if (ARG1.uge(ARG0.getBitWidth()))
        return EvalValue::poison();
      auto Res = ARG0.lshr(ARG1);
      if (ARG0 != Res.shl(ARG1))
        return EvalValue::poison();
      return Res;
    }

    case Inst::AShr:
      if (ARG1.uge(ARG0.getBitWidth()))
        return EvalValue::poison();
      return {ARG0.ashr(ARG1)};

    case Inst::AShrExact:{
      if (ARG1.uge(ARG0.getBitWidth()))
        return EvalValue::poison();
      auto Res = ARG0.ashr(ARG1);
      if (ARG0 != Res.shl(ARG1))
        return EvalValue::poison();
      return Res;
    }

    case Inst::Select:
      if (!Args[0].hasValue())
        return EvalValue::ub();
      return ARG0.getBoolValue() ? Args[1] : Args[2];

    case Inst::ZExt:
      return {ARG0.zext(Inst->Width)};

    case Inst::SExt:
      return {ARG0.sext(Inst->Width)};

    case Inst::Trunc:
      return {ARG0.trunc(Inst->Width)};

    case Inst::Eq:
      return {{1, ARG0 == ARG1}};

    case Inst::Ne:
      return {{1, ARG0 != ARG1}};

    case Inst::Ult:
      return {{1, ARG0.ult(ARG1)}};

    case Inst::Slt:
      return {{1, ARG0.slt(ARG1)}};

    case Inst::Ule:
      return {{1, ARG0.ule(ARG1)}};

    case Inst::Sle:
      return {{1, ARG0.sle(ARG1)}};

    case Inst::CtPop:
      return {llvm::APInt(Inst->Width, ARG0.countPopulation())};

    case Inst::Ctlz:
      return {llvm::APInt(Inst->Width,
                          ARG0.countLeadingZeros())};

    case Inst::Cttz:
      return {llvm::APInt(Inst->Width,
                          ARG0.countTrailingZeros())};

    case Inst::BSwap:
      return {ARG0.byteSwap()};

    case Inst::BitReverse:
      return {ARG0.reverseBits()};

    case Inst::FShl:{
      unsigned W = ARG0.getBitWidth();
      auto Input = (ARG0.zext(2 * W).shl(W)) | ARG1.zext(2 * W);
      auto Output = Input.shl(ARG2.urem(W));
      return {Output.extractBits(W, W)};
    }

    case Inst::FShr:{
      unsigned W = ARG0.getBitWidth();
      auto Input = (ARG0.zext(2 * W).shl(W)) | ARG1.zext(2 * W);
      auto Output = Input.lshr(ARG2.urem(W));
      return {Output.extractBits(W, 0)};
    }

    // TODO implement these
    case Inst::ExtractValue:
    case Inst::SAddWithOverflow:
    case Inst::SAddO:
    case Inst::UAddWithOverflow:
    case Inst::UAddO:
    case Inst::SSubWithOverflow:
    case Inst::SSubO:
    case Inst::USubWithOverflow:
    case Inst::USubO:
    case Inst::SMulWithOverflow:
    case Inst::SMulO:
    case Inst::UMulWithOverflow:
    case Inst::UMulO:
      return {EvalValue()};

    default:
      llvm::report_fatal_error("unimplemented instruction kind " +
                               std::string(Inst::getKindName(Inst->K)) +
                               " in interpreter");
    }
  }

#undef ARG0
#undef ARG1
#undef ARG2

  EvalValue evaluateInst(Inst *Root, ValueCache &Cache) {
    // TODO populate cache and look things up in it
    // needed?
    if (Root->K == Inst::Var) {
      return Cache[Root];
    } else {
      // TODO SmallVector
      std::vector<EvalValue> EvaluatedArgs;
      for (auto &&I : Root->Ops)
        EvaluatedArgs.push_back(evaluateInst(I, Cache));
      auto Result = evaluateSingleInst(Root, EvaluatedArgs);
      //Cache[Root] = Result;
      return Result;
    }
  }

  EvalValue getValue(Inst *I, ValueCache &C) {
    if (I->K == Inst::Const)
      return {I->Val};
    if (I->K == Inst::Var ||
        I->K == Inst::ReservedConst ||
        I->K == Inst::ReservedInst) {
      if (C.find(I) != C.end())
        return C[I];
    }
    // unimplemented
    return EvalValue();
  }

#define KB0 findKnownBits(I->Ops[0], C, PartialEval)
#define KB1 findKnownBits(I->Ops[1], C, PartialEval)

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
      EvalValue V = getValue(I, C);
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
      auto Op1V = getValue(I->Ops[1], C);
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
      auto Op1V = getValue(I->Ops[1], C);

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
      } else if (I->Ops[1]->Name.find(ReservedConstPrefix) != std::string::npos) {
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
      auto Op1V = getValue(I->Ops[1], C);
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
//   case Eq:
//     return "eq";
//   case Ne:
//     return "ne";
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
      EvalValue V = getValue(I, C);
      if (V.hasValue()) {
        return llvm::ConstantRange(V.getValue());
      } else {
        if (I->Name.find(ReservedConstPrefix) != std::string::npos) {
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
      auto V1 = getValue(I->Ops[1], C);
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
}
