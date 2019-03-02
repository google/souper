#include "souper/Infer/Interpreter.h"

namespace souper {

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

  llvm::KnownBits findKnownBits(Inst *I, ValueCache &C) {
    llvm::KnownBits Result(I->Width);
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
    case Inst::Shl : {
      auto Op0KB = findKnownBits(I->Ops[0], C);
      auto Op1V = getValue(I->Ops[1], C);
      if (Op1V.hasValue()) {
        Op0KB.One <<= Op1V.getValue();
        Op0KB.Zero <<= Op1V.getValue();
        Op0KB.Zero.setLowBits(Op1V.getValue().getLimitedValue());
        // setLowBits takes an unsiged int, so getLimitedValue is harmless
        return Op0KB;
      } else {
        return Result;
      }
    }
    case Inst::And : {
      auto Op0KB = findKnownBits(I->Ops[0], C);
      auto Op1KB = findKnownBits(I->Ops[1], C);

      Op0KB.One &= Op1KB.One;
      Op0KB.Zero |= Op1KB.Zero;
      return Op0KB;
    }
    case Inst::Or : {
      auto Op0KB = findKnownBits(I->Ops[0], C);
      auto Op1KB = findKnownBits(I->Ops[1], C);

      Op0KB.One |= Op1KB.One;
      Op0KB.Zero &= Op1KB.Zero;
      return Op0KB;
    }
    case Inst::Xor : {
      auto Op0KB = findKnownBits(I->Ops[0], C);
      auto Op1KB = findKnownBits(I->Ops[1], C);
      llvm::APInt KnownZeroOut =
        (Op0KB.Zero & Op1KB.Zero) | (Op0KB.One & Op1KB.One);
      Op0KB.One = (Op0KB.Zero & Op1KB.One) | (Op0KB.One & Op1KB.Zero);
      Op0KB.Zero = std::move(KnownZeroOut);
      // ^ logic copied from LLVM ValueTracking.cpp
      return Op0KB;
    }
    default :
      return Result;
    }
  }

  llvm::ConstantRange findConstantRange(Inst *I,
                                        ValueCache &C) {
    llvm::ConstantRange result(I->Width);
    switch (I->K) {
    case Inst::Const:
    case Inst::Var : {
      EvalValue V = getValue(I, C);
      if (V.hasValue()) {
        return llvm::ConstantRange(V.getValue());
      } else {
        return result; // Whole range
      }
    }
    case Inst::Trunc: {
      auto R0 = findConstantRange(I->Ops[0], C);
      return R0.truncate(I->Width);
    }
    case Inst::SExt: {
      auto R0 = findConstantRange(I->Ops[0], C);
      return R0.signExtend(I->Width);
    }
    case Inst::ZExt: {
      auto R0 = findConstantRange(I->Ops[0], C);
      return R0.zeroExtend(I->Width);
    }
    case Inst::Add: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.add(R1);
    }
    case Inst::AddNSW: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto V1 = getValue(I->Ops[1], C);
      if (V1.hasValue()) {
        return R0.addWithNoSignedWrap(V1.getValue());
      } else {
        return result; // full range, can we do better?
      }
    }
    case Inst::Sub: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.sub(R1);
    }
    case Inst::Mul: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.multiply(R1);
    }
    case Inst::And: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.binaryAnd(R1);
    }
    case Inst::Or: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.binaryOr(R1);
    }
    case Inst::Shl: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.shl(R1);
    }
    case Inst::AShr: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.ashr(R1);
    }
    case Inst::LShr: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.lshr(R1);
    }
    case Inst::UDiv: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.udiv(R1);
    }
      //     case Inst::SDiv: {
      //       auto R0 = FindConstantRange(I->Ops[0], C);
      //       auto R1 = FindConstantRange(I->Ops[1], C);
      //       return R0.sdiv(R1); // unimplemented
      //     }
      // TODO: Xor pattern for not, truncs and extends, etc
    default:
      return result;
    }
  }

}
