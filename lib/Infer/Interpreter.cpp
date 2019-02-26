#include "souper/Infer/Interpreter.h"

namespace souper {
namespace {
EvalValue getValue(Inst *I, ValueCache &C) {
  if (I->K == souper::Inst::Const) {
      return {I->Val};
  } else if (I->K == souper::Inst::Var
      || I->K == souper::Inst::ReservedConst
      || I->K == souper::Inst::ReservedInst) {
    if (I->Name != "" && C.find(I) != C.end()) {
      return C[I];
    } else {
      return EvalValue(); // unimplemented
    }
  }
  return EvalValue();
}
}


EvalValue evaluateSingleInst(souper::Inst* Inst,
                             std::vector<EvalValue> args) {
  switch (Inst->K) {
    case souper::Inst::Const:
      return {Inst->Val};
    case souper::Inst::UntypedConst:
      return {Inst->Val};
    case souper::Inst::Var:
      llvm_unreachable("Should get value from cache without reaching here");
    case souper::Inst::Add:
      return {args[0].getValue() + args[1].getValue()};
    case souper::Inst::Sub:
      return {args[0].getValue() - args[1].getValue()};
    case souper::Inst::Mul:
      return {args[0].getValue() * args[1].getValue()};
    case souper::Inst::UDiv:
      if (args[1].getValue() == 0) {
        return EvalValue::ub();
      }
      return {args[0].getValue().udiv(args[1].getValue())};
    case souper::Inst::SDiv:
      if (args[1].getValue() == 0) {
        return EvalValue::ub();
      }
      return {args[0].getValue().sdiv(args[1].getValue())};
    case souper::Inst::URem:
      return {args[0].getValue().urem(args[1].getValue())};
    case souper::Inst::SRem:
      return {args[0].getValue().srem(args[1].getValue())};
    case souper::Inst::And:
      return {args[0].getValue() & args[1].getValue()};
    case souper::Inst::Or:
      return {args[0].getValue() | args[1].getValue()};
    case souper::Inst::Xor:
      return {args[0].getValue() ^ args[1].getValue()};
    case souper::Inst::Shl:
      return {args[0].getValue() << args[1].getValue()};
    case souper::Inst::LShr:
      return {args[0].getValue().lshr(args[1].getValue())};
    case souper::Inst::AShr:
      return {args[0].getValue().ashr(args[1].getValue())};
    // TODO: Handle NSW/NUW/NW variants of instructions
    case souper::Inst::Select: {
      if (!args[0].hasValue()) {
        return args[0];
      } else {
        bool cond = args[0].getValue().getBoolValue();
        return cond ? args[1] : args[2];
      }
    }
    case souper::Inst::ZExt:
      return {args[0].getValue().zext(Inst->Width)};
    case souper::Inst::SExt:
      return {args[0].getValue().sext(Inst->Width)};
    case souper::Inst::Trunc:
      return {args[0].getValue().trunc(Inst->Width)};
    case souper::Inst::Eq:
      return {{1, args[0].getValue() == args[1].getValue()}};
    case souper::Inst::Ne:
      return {{1, args[0].getValue() != args[1].getValue()}};
    case souper::Inst::Ult:
      return {{1, args[0].getValue().ult(args[1].getValue())}};
    case souper::Inst::Slt:
      return {{1, args[0].getValue().slt(args[1].getValue())}};
    case souper::Inst::Ule:
      return {{1, args[0].getValue().ule(args[1].getValue())}};
    case souper::Inst::Sle:
      return {{1, args[0].getValue().sle(args[1].getValue())}};
    case souper::Inst::CtPop:
      return {llvm::APInt(Inst->Width, args[0].getValue().countPopulation())};
    case souper::Inst::Ctlz:
      return {llvm::APInt(Inst->Width,
                          args[0].getValue().countLeadingZeros())};
    case souper::Inst::Cttz:
      return {llvm::APInt(Inst->Width,
                          args[0].getValue().countTrailingZeros())};
    case souper::Inst::BSwap:
      return {args[0].getValue().byteSwap()};
    default:
      return EvalValue(); // unimplemented
  }
}

// Errs on the side of an 'unimplemented' result
// TODO: Some instructions unimplemented
EvalValue evaluateInst(souper::Inst* Root, ValueCache &Cache) {
  std::vector<EvalValue> EvaluatedArgs;
  EvalValue Result;

  if (Root->K == souper::Inst::Var) {
      Result = Cache[Root];
  } else {
    for (auto &&I : Root->Ops) {
      auto ArgEval = evaluateInst(I, Cache);
      if (!ArgEval.hasValue() && Root->K != souper::Inst::Select) {
        return ArgEval;
        // result has the same ValueKind as the argument which
        // failed to produce a ValueKind::Val
      }
      EvaluatedArgs.push_back(ArgEval);
    }
    Result = evaluateSingleInst(Root, EvaluatedArgs);
  }
  return Result;
}

llvm::KnownBits findKnownBits(Inst* I, ValueCache& C) {
  llvm::KnownBits Result(I->Width);
  switch(I->K) {
    case souper::Inst::Const:
    case souper::Inst::Var : {
      EvalValue V = getValue(I, C);
      if (V.hasValue()) {
        Result.One = V.getValue();
        Result.Zero = ~V.getValue();
        return Result;
      } else {
        return Result;
      }
    }
    case souper::Inst::Shl : {
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
    case souper::Inst::And : {
      auto Op0KB = findKnownBits(I->Ops[0], C);
      auto Op1KB = findKnownBits(I->Ops[1], C);

      Op0KB.One &= Op1KB.One;
      Op0KB.Zero |= Op1KB.Zero;
      return Op0KB;
    }
    case souper::Inst::Or : {
      auto Op0KB = findKnownBits(I->Ops[0], C);
      auto Op1KB = findKnownBits(I->Ops[1], C);

      Op0KB.One |= Op1KB.One;
      Op0KB.Zero &= Op1KB.Zero;
      return Op0KB;
    }
    case souper::Inst::Xor : {
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

llvm::ConstantRange findConstantRange(souper::Inst* I,
                                      souper::ValueCache& C) {
  llvm::ConstantRange result(I->Width);
  switch (I->K) {
    case souper::Inst::Const:
    case souper::Inst::Var : {
      EvalValue V = getValue(I, C);
      if (V.hasValue()) {
        return llvm::ConstantRange(V.getValue());
      } else {
        return result; // Whole range
      }
    }
    case souper::Inst::Trunc: {
      auto R0 = findConstantRange(I->Ops[0], C);
      return R0.truncate(I->Width);
    }
    case souper::Inst::SExt: {
      auto R0 = findConstantRange(I->Ops[0], C);
      return R0.signExtend(I->Width);
    }
    case souper::Inst::ZExt: {
      auto R0 = findConstantRange(I->Ops[0], C);
      return R0.zeroExtend(I->Width);
    }
    case souper::Inst::Add: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.add(R1);
    }
    case souper::Inst::AddNSW: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto V1 = getValue(I->Ops[1], C);
      if (V1.hasValue()) {
        return R0.addWithNoSignedWrap(V1.getValue());
      } else {
        return result; // full range, can we do better?
      }
    }
    case souper::Inst::Sub: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.sub(R1);
    }
    case souper::Inst::Mul: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.multiply(R1);
    }
    case souper::Inst::And: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.binaryAnd(R1);
    }
    case souper::Inst::Or: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.binaryOr(R1);
    }
    case souper::Inst::Shl: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.shl(R1);
    }
    case souper::Inst::AShr: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.ashr(R1);
    }
    case souper::Inst::LShr: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.lshr(R1);
    }
    case souper::Inst::UDiv: {
      auto R0 = findConstantRange(I->Ops[0], C);
      auto R1 = findConstantRange(I->Ops[1], C);
      return R0.udiv(R1);
    }
//     case souper::Inst::SDiv: {
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
