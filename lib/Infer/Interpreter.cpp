#include "souper/Infer/Interpreter.h"

namespace souper {
#define ARG0 Args[0].getValue()
#define ARG1 Args[1].getValue()
#define ARG2 Args[2].getValue()

  EvalValue evaluateAddNSW(llvm::APInt a, llvm::APInt b) {
    bool Ov;
    auto Res = a.sadd_ov(b, Ov);
    if (Ov)
      return EvalValue::poison();
    else
      return Res;
  }

  EvalValue evaluateAddNUW(llvm::APInt a, llvm::APInt b) {
    bool Ov;
    auto Res = a.uadd_ov(b, Ov);
    if (Ov)
      return EvalValue::poison();
    else
      return Res;
  }

  EvalValue evaluateAddNW(llvm::APInt a, llvm::APInt b) {
    bool Ov1, Ov2;
    auto Res1 = a.sadd_ov(b, Ov1);
    auto Res2 = a.uadd_ov(b, Ov2);
    if (Ov1 || Ov2)
      return EvalValue::poison();
    else
      return Res1;
  }

  EvalValue evaluateSubNSW(llvm::APInt a, llvm::APInt b) {
    bool Ov;
    auto Res = a.ssub_ov(b, Ov);
    if (Ov)
      return EvalValue::poison();
    else
      return Res;
  }

  EvalValue evaluateSubNUW(llvm::APInt a, llvm::APInt b) {
    bool Ov;
    auto Res = a.usub_ov(b, Ov);
    if (Ov)
      return EvalValue::poison();
    else
      return Res;
  }

  EvalValue evaluateSubNW(llvm::APInt a, llvm::APInt b) {
    bool Ov1, Ov2;
    auto Res1 = a.ssub_ov(b, Ov1);
    auto Res2 = a.usub_ov(b, Ov2);
    if (Ov1 || Ov2)
      return EvalValue::poison();
    else
      return Res1;
  }

  EvalValue evaluateUDiv(llvm::APInt a, llvm::APInt b) {
    if (b == 0)
      return EvalValue::ub();
    return {a.udiv(b)};
  }

  EvalValue evaluateURem(llvm::APInt a, llvm::APInt b) {
    if (b == 0)
      return EvalValue::ub();
    return {a.urem(b)};
  }

  EvalValue evaluateShl(llvm::APInt a, llvm::APInt b) {
    if (b.uge(a.getBitWidth()))
      return EvalValue::poison();
    return {a << b};
  }

  EvalValue evaluateLShr(llvm::APInt a, llvm::APInt b) {
    if (b.uge(a.getBitWidth()))
      return EvalValue::poison();
    return {a.lshr(b)};
  }

  EvalValue evaluateAShr(llvm::APInt a, llvm::APInt b) {
    if (b.uge(a.getBitWidth()))
      return EvalValue::poison();
    return {a.ashr(b)};
  }

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

    case Inst::AddNSW:
      return evaluateAddNSW(ARG0, ARG1);

    case Inst::AddNUW:
      return evaluateAddNUW(ARG0, ARG1);

    case Inst::AddNW:
      return evaluateAddNW(ARG0, ARG1);

    case Inst::Sub:
      return {ARG0 - ARG1};

    case Inst::SubNSW:
      return evaluateSubNSW(ARG0, ARG1);

    case Inst::SubNUW:
      return evaluateSubNUW(ARG0, ARG1);

    case Inst::SubNW:
      return evaluateSubNW(ARG0, ARG1);

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
      return evaluateUDiv(ARG0, ARG1);

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
      return evaluateURem(ARG0, ARG1);

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
      return evaluateShl(ARG0, ARG1);

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
      return evaluateLShr(ARG0, ARG1);

    case Inst::LShrExact:{
      if (ARG1.uge(ARG0.getBitWidth()))
        return EvalValue::poison();
      auto Res = ARG0.lshr(ARG1);
      if (ARG0 != Res.shl(ARG1))
        return EvalValue::poison();
      return Res;
    }

    case Inst::AShr:
      return evaluateAShr(ARG0, ARG1);

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
}
