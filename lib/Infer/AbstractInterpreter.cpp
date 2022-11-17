// Copyright 2019 The Souper Authors. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "souper/Extractor/Solver.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Util/LLVMUtils.h"

#include "llvm/IR/Operator.h"

using namespace llvm;

namespace {
  using souper::getSetSize;

  APInt getUMin(const KnownBits &X) { return X.One; }

  APInt getUMax(const KnownBits &X) { return ~X.Zero; }

  bool isSignKnown(const KnownBits &X) {
    unsigned W = X.getBitWidth();
    return X.One[W - 1] || X.Zero[W - 1];
  }

  APInt getSMin(const KnownBits &X) {
    if (isSignKnown(X))
      return X.One;
    APInt Min = X.One;
    Min.setBit(X.getBitWidth() - 1);
    return Min;
  }

  APInt getSMax(const KnownBits &X) {
    if (isSignKnown(X))
      return ~X.Zero;
    APInt Max = ~X.Zero;
    Max.clearBit(X.getBitWidth() - 1);
    return Max;
  }

} // anonymous

namespace souper {

  bool KnownBitsAnalysis::isConflictingKB(const KnownBits &A, const KnownBits &B) {
    return ((A.One & B.Zero) != 0) || ((A.Zero & B.One) != 0);
  }

  KnownBits KnownBitsAnalysis::getMostPreciseKnownBits(KnownBits A, KnownBits B) {
    unsigned unknownCountA =
      A.getBitWidth() - (A.Zero.countPopulation() + A.One.countPopulation());
    unsigned unknownCountB =
      B.getBitWidth() - (B.Zero.countPopulation() + B.One.countPopulation());
    return unknownCountA < unknownCountB ? A : B;
  }

  std::string KnownBitsAnalysis::knownBitsString(llvm::KnownBits KB) {
    std::string S = "";
    for (int I = 0; I < KB.getBitWidth(); I++) {
      if (KB.Zero.isNegative())
        S += "0";
      else if (KB.One.isNegative())
        S += "1";
      else
        S += "?";
      KB.Zero <<= 1;
      KB.One <<= 1;
    }
    return S;
  }

  llvm::KnownBits concatKnownBits(llvm::KnownBits A, llvm::KnownBits B) {
    auto W = A.Zero.getBitWidth() + B.Zero.getBitWidth();
    llvm::KnownBits Result(W);
    Result.Zero = (A.Zero.zext(W) << B.getBitWidth()) | B.Zero.zext(W);
    Result.One = (A.One.zext(W) << B.getBitWidth()) | B.One.zext(W);
    return Result;
  }

  namespace BinaryTransferFunctionsKB {
    llvm::KnownBits add(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/true, /*NSW=*/false,
                                               LHS, RHS);
    }

    llvm::KnownBits addnsw(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/true, /*NSW=*/true,
                                               LHS, RHS);
    }

    llvm::KnownBits sub(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/false, /*NSW=*/false,
                                               LHS, RHS);
    }

    llvm::KnownBits subnsw(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/false, /*NSW=*/true,
                                               LHS, RHS);
    }

    llvm::KnownBits mul(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(LHS.getBitWidth());

      // TODO: Below only takes into account leading and trailing zeros. Maybe
      // also do something with leading ones or trailing ones for improvement?
      auto trailingZeros0 = LHS.countMinTrailingZeros();
      auto trailingZeros1 = RHS.countMinTrailingZeros();
      Result.Zero.setLowBits(std::min(trailingZeros0 + trailingZeros1, LHS.getBitWidth()));

      // check for leading zeros
      auto lz0 = LHS.countMinLeadingZeros();
      auto lz1 = RHS.countMinLeadingZeros();
      auto confirmedLeadingZeros = lz0 + lz1 - 1;
      auto resultSize = LHS.getBitWidth() + RHS.getBitWidth() - 1;
      if (resultSize - confirmedLeadingZeros < LHS.getBitWidth())
        Result.Zero.setHighBits(LHS.getBitWidth() - (resultSize - confirmedLeadingZeros));

      // two numbers odd means reuslt is odd
      if (LHS.One[0] && RHS.One[0])
        Result.One.setLowBits(1);

      return Result;
    }

    llvm::KnownBits udiv(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(LHS.getBitWidth());
      const auto width = Result.getBitWidth();

      unsigned LeadZ = LHS.countMinLeadingZeros();
      unsigned RHSMaxLeadingZeros = RHS.countMaxLeadingZeros();
      if (RHSMaxLeadingZeros != width)
        LeadZ = std::min(width, LeadZ + width - RHSMaxLeadingZeros - 1);
      Result.Zero.setHighBits(LeadZ);
      return Result;
    }

    llvm::KnownBits urem(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(LHS.getBitWidth());
      const auto width = Result.getBitWidth();

      if (RHS.isConstant()) {
        auto RA = RHS.getConstant();
        if (RA.isPowerOf2()) {
          auto LowBits = (RA - 1);
          Result = LHS;
          Result.Zero |= ~LowBits;
          Result.One &= LowBits;
          return Result;
        }
      }

      unsigned Leaders =
        std::max(LHS.countMinLeadingZeros(), RHS.countMinLeadingZeros());
      Result.resetAll();
      Result.Zero.setHighBits(Leaders);
      return Result;
    }

    llvm::KnownBits and_(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      auto result = LHS;
      result.One &= RHS.One;
      result.Zero |= RHS.Zero;
      return result;
    }

    llvm::KnownBits or_(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      auto result = LHS;
      result.One |= RHS.One;
      result.Zero &= RHS.Zero;
      return result;
    }

    llvm::KnownBits xor_(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      auto result = LHS;
      llvm::APInt KnownZeroOut =
        (LHS.Zero & RHS.Zero) | (LHS.One & RHS.One);
      result.One = (LHS.Zero & RHS.One) | (LHS.One & RHS.Zero);
      result.Zero = std::move(KnownZeroOut);
      // ^ logic copied from LLVM ValueTracking.cpp
      return result;
    }

    llvm::KnownBits shl(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(LHS.getBitWidth());
      const auto width = Result.getBitWidth();

      auto Op0KB = LHS;
      if (RHS.isConstant()) {
        auto Val = RHS.getConstant().getLimitedValue();
        if (Val < 0 || Val >= width) {
          return Result;
        }

        Op0KB.One <<= Val;
        Op0KB.Zero <<= Val;
        Op0KB.Zero.setLowBits(Val);
        // setLowBits takes an unsigned int, so getLimitedValue is harmless
        return Op0KB;
      }

      unsigned minValue = RHS.One.getLimitedValue();
      Result.Zero.setLowBits(std::min(LHS.countMinTrailingZeros() + minValue, width));

      return Result;
    }

    llvm::KnownBits lshr(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(LHS.getBitWidth());
      const auto width = Result.getBitWidth();

      auto Op0KB = LHS;
      if (RHS.isConstant()) {
        auto Val = RHS.getConstant().getLimitedValue();
        if (Val < 0 || Val >= width) {
          return Result;
        }
        Op0KB.One.lshrInPlace(Val);
        Op0KB.Zero.lshrInPlace(Val);
        Op0KB.Zero.setHighBits(Val);
        // setHighBits takes an unsigned int, so getLimitedValue is harmless
        return Op0KB;
      }

      unsigned minValue = RHS.One.getLimitedValue();
      Result.Zero.setHighBits(std::min(minValue + LHS.countMinLeadingZeros(), width));

      return Result;
    }

    llvm::KnownBits ashr(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(LHS.getBitWidth());
      const auto width = Result.getBitWidth();

      unsigned minValue = RHS.One.getLimitedValue();
      if (LHS.One.isSignBitSet()) {
        // confirmed: sign bit = 1
        Result.One.setHighBits(std::min(LHS.countMinLeadingOnes() + minValue, width));
      } else if (LHS.Zero.isSignBitSet()) {
        // confirmed: sign bit = 0
        Result.Zero.setHighBits(std::min(LHS.countMinLeadingZeros() + minValue, width));
      }
      return Result;
    }

    llvm::KnownBits eq(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(1);
      if (LHS.isConstant() && RHS.isConstant() && (LHS.getConstant() == RHS.getConstant())) {
        Result.One.setBit(0);
        return Result;
      }
      if (((LHS.One & RHS.Zero) != 0) || ((LHS.Zero & RHS.One) != 0)) {
        Result.Zero.setBit(0);
        return Result;
      }
      return Result;
    }

    llvm::KnownBits ne(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(1);

      if (LHS.isConstant() && RHS.isConstant() && (LHS.getConstant() == RHS.getConstant()))
        Result.Zero.setBit(0);
      if (((LHS.One & RHS.Zero) != 0) || ((LHS.Zero & RHS.One) != 0))
        Result.One.setBit(0);
      return Result;
    }

    llvm::KnownBits ult(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(1);
      if (getUMax(LHS).ult(getUMin(RHS)))
        Result.One.setBit(0);
      if (getUMin(LHS).uge(getUMax(RHS)))
        Result.Zero.setBit(0);
      return Result;
    }

    llvm::KnownBits slt(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(1);
      if (getSMax(LHS).slt(getSMin(RHS)))
        Result.One.setBit(0);
      if (getSMin(LHS).sge(getSMax(RHS)))
        Result.Zero.setBit(0);
      return Result;
    }

    llvm::KnownBits ule(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(1);
      if (getUMax(LHS).ule(getUMin(RHS)))
        Result.One.setBit(0);
      if (getUMin(LHS).ugt(getUMax(RHS)))
        Result.Zero.setBit(0);
      return Result;
    }

    llvm::KnownBits sle(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS) {
      llvm::KnownBits Result(1);
      if (getSMax(LHS).sle(getSMin(RHS)))
        Result.One.setBit(0);
      if (getSMin(LHS).sgt(getSMax(RHS)))
        Result.Zero.setBit(0);
      return Result;
    }

  } // ns BinaryTransferFunctionsKB


  namespace BinaryTransferFunctionsCR {
    llvm::ConstantRange binaryOr(const llvm::ConstantRange &LHS, const llvm::ConstantRange &RHS) {
      if (LHS.isEmptySet() || RHS.isEmptySet())
        return llvm::ConstantRange(LHS.getBitWidth(), /*isFullSet=*/false);

      APInt umax = APIntOps::umax(LHS.getUnsignedMin(), RHS.getUnsignedMin());
      APInt res = APInt::getNullValue(LHS.getBitWidth());
      if (!LHS.isWrappedSet() && !LHS.isUpperWrapped() &&
          !RHS.isWrappedSet() && !RHS.isUpperWrapped()) {
        APInt umaxupper = APIntOps::umax(LHS.getUnsignedMax(), RHS.getUnsignedMax());
        APInt uminupper = APIntOps::umin(LHS.getUnsignedMax(), RHS.getUnsignedMax());
        res = APInt::getLowBitsSet(LHS.getBitWidth(),
                                   LHS.getBitWidth() - uminupper.countLeadingZeros());
        res = res | umaxupper;
        res = res + 1;
      }

      if (umax != res)
        return llvm::ConstantRange(std::move(umax), std::move(res));

      return llvm::ConstantRange(LHS.getBitWidth(), /*isFullSet=*/true);
    }

    llvm::ConstantRange binaryAnd(const llvm::ConstantRange &LHS, const llvm::ConstantRange &RHS) {
      if (LHS.isEmptySet() || RHS.isEmptySet())
        return llvm::ConstantRange(LHS.getBitWidth(), /*isFullSet=*/false);

      APInt umin = APIntOps::umin(RHS.getUnsignedMax(), LHS.getUnsignedMax());
      if (umin.isAllOnesValue())
        return llvm::ConstantRange(LHS.getBitWidth(), /*isFullSet=*/true);

      APInt res = APInt::getNullValue(LHS.getBitWidth());

      const APInt upper1 = LHS.getUnsignedMax();
      const APInt upper2 = RHS.getUnsignedMax();
      APInt lower1 = LHS.getUnsignedMin();
      APInt lower2 = RHS.getUnsignedMin();
      const APInt tmp = lower1 & lower2;
      const unsigned bitPos = LHS.getBitWidth() - tmp.countLeadingZeros();
      // if there are no zeros from bitPos upto both barriers, lower bound have bit
      // set at bitPos. Barrier is the point beyond which you cannot set the bit
      // because it will be greater than the upper bound then
      if (!LHS.isWrappedSet() && !LHS.isUpperWrapped() &&
          !RHS.isWrappedSet() && !RHS.isUpperWrapped() &&
          (lower1.countLeadingZeros() == upper1.countLeadingZeros()) &&
          (lower2.countLeadingZeros() == upper2.countLeadingZeros()) &&
          bitPos > 0) {
        lower1.lshrInPlace(bitPos - 1);
        lower2.lshrInPlace(bitPos - 1);
        if (lower1.countTrailingOnes() == (LHS.getBitWidth() - lower1.countLeadingZeros()) &&
            lower2.countTrailingOnes() == (LHS.getBitWidth() - lower2.countLeadingZeros())) {
          res = APInt::getOneBitSet(LHS.getBitWidth(), bitPos - 1);
        }
      }

      return llvm::ConstantRange(std::move(res), std::move(umin) + 1);
    }

  } // ns BinaryTransferFunctionsCR

  bool isReservedConst(Inst *I) {
    return I->K == Inst::ReservedConst ||
      (I->K == Inst::Var && I->SynthesisConstID != 0);
  }

  bool isHole(Inst *I) {
    return I->K == Inst::Hole;
  }

  bool isConcrete(Inst *I, bool ConsiderConsts, bool ConsiderHoles) {
    std::vector<Inst *> Insts;
    if (I->nReservedConsts == -1) {
      Insts.clear();
      findInsts(I, Insts, [](Inst *instr) {
        return isReservedConst(instr);
      });
      I->nReservedConsts = Insts.size();
    }

    if (I->nHoles == -1) {
      Insts.clear();
      findInsts(I, Insts, [](Inst *instr) {
        return isHole(instr);
      });
      I->nHoles = Insts.size();
    }

    bool retval = true;
    if (ConsiderConsts)
      retval &= I->nReservedConsts == 0;
    if (ConsiderHoles)
      retval &= I->nHoles == 0;

    return retval;
  }

  // Tries to get the concrete value from @I
  EvalValue getValue(Inst *I, ConcreteInterpreter &CI) {
    if (I->K == Inst::Const)
      return {I->Val};
    else if (I->K == Inst::Var && !isReservedConst(I))
      // evaluateInst will only give us input value of the variable; it doesn't
      // evaluate anything.
      return CI.evaluateInst(I);

    if (isConcrete(I))
      return CI.evaluateInst(I);

    // unimplemented
    return EvalValue();
  }

#define KB0 findKnownBits(I->Ops[0], CI, UsePartialEval)
#define KB1 findKnownBits(I->Ops[1], CI, UsePartialEval)
#define KB2 findKnownBits(I->Ops[2], CI, UsePartialEval)
#define VAL(INST) getValue(INST, CI)

  llvm::KnownBits KnownBitsAnalysis::mergeKnownBits(std::vector<llvm::KnownBits> Vec) {
    assert(Vec.size() > 0);

    auto Width = Vec[0].getBitWidth();
#ifndef NDEBUG
    for (unsigned i = 1; i < Vec.size(); i++) {
      if (Width != Vec[i].getBitWidth())
        llvm::report_fatal_error("mergeKnownBits: bitwidth should be same of all inputs");
    }
#endif

    APInt OneResult = APInt::getAllOnesValue(Width);
    APInt ZeroResult = APInt::getAllOnesValue(Width);
    for (unsigned i = 0; i < Vec.size(); i++) {
      OneResult &= Vec[i].One;
      ZeroResult &= Vec[i].Zero;
    }

    KnownBits Result(Width);
    Result.One = OneResult;
    Result.Zero = ZeroResult;

    return Result;
  }

  bool KnownBitsAnalysis::cacheHasValue(Inst *I) {
    if (KBCache.find(I) != KBCache.end())
      return true;

    if (I->K == Inst::Var && (I->KnownZeros.getBoolValue() || I->KnownOnes.getBoolValue())) {
      llvm::KnownBits metadataKB;
      metadataKB.Zero = I->KnownZeros;
      metadataKB.One = I->KnownOnes;

      KBCache.emplace(I, std::move(metadataKB));
      return true;
    }

    return false;
  }

  llvm::KnownBits KnownBitsAnalysis::findKnownBits(Inst *I, ConcreteInterpreter &CI, bool UsePartialEval) {
    llvm::KnownBits Result(I->Width);

    if (cacheHasValue(I))
      return KBCache.at(I);

    if (UsePartialEval || I->K == Inst::Const) {
    EvalValue V = VAL(I);
    if (V.hasValue()) {
      Result.One = V.getValue();
      Result.Zero = ~V.getValue();

      // cache before returning
      KBCache.emplace(I, Result);

      return Result;
    }
    }


    switch(I->K) {
    case Inst::Freeze: {
      Result = KB0;
      break;
    }
    case Inst::Phi: {
      std::vector<llvm::KnownBits> vec;
      for (auto &Op : I->Ops) {
        vec.emplace_back(findKnownBits(Op, CI, UsePartialEval));
      }
      Result = mergeKnownBits(vec);
      break;
    }
    case Inst::AddNUW :
    case Inst::AddNW :
    case Inst::Add:
      Result = BinaryTransferFunctionsKB::add(KB0, KB1);
      break;
    case Inst::AddNSW:
      Result = BinaryTransferFunctionsKB::addnsw(KB0, KB1);
      break;
    case Inst::SubNUW :
    case Inst::SubNW :
    case Inst::Sub:
      Result = BinaryTransferFunctionsKB::sub(KB0, KB1);
      break;
    case Inst::SubNSW:
      Result = BinaryTransferFunctionsKB::subnsw(KB0, KB1);
      break;
    case Inst::Mul:
    case Inst::MulNSW:
    case Inst::MulNUW:
    case Inst::MulNW:
      Result = BinaryTransferFunctionsKB::mul(KB0, KB1);
      break;
    case Inst::UDiv:
      Result = BinaryTransferFunctionsKB::udiv(KB0, KB1);
      break;
//   case SDiv:
//     return "sdiv";
//   case UDivExact:
//     return "udivexact";
//   case SDivExact:
//     return "sdivexact";
    case Inst::URem:
      Result = BinaryTransferFunctionsKB::urem(KB0, KB1);
      break;
//   case SRem:
//     return "srem";
    case Inst::And :
      Result = BinaryTransferFunctionsKB::and_(KB0, KB1);
      break;
    case Inst::Or :
      Result = BinaryTransferFunctionsKB::or_(KB0, KB1);
      break;
    case Inst::Xor :
      Result = BinaryTransferFunctionsKB::xor_(KB0, KB1);
      break;
    case Inst::ShlNSW :
    case Inst::ShlNUW :
    case Inst::ShlNW : // TODO: Rethink if these make sense
    case Inst::Shl : {
      // we can't easily put following condition inside
      // BinaryTransferFunctionsKB but this one gives significant pruning; so,
      // let's keep it here.
      // Note that only code inside BinaryTransferFunctionsKB is testable from
      // unit tests. Put minimum code outside it which you are sure of being
      // correct.
      // Synthesized constant cannot be zero.
      if (isReservedConst(I->Ops[1]))
        Result.Zero.setLowBits(1);
      Result = getMostPreciseKnownBits(Result, BinaryTransferFunctionsKB::shl(KB0, KB1));
      break;
    }
    case Inst::LShr : {
      // Synthesized constant cannot be zero.
      if (isReservedConst(I->Ops[1]))
        Result.Zero.setHighBits(1);
      Result = getMostPreciseKnownBits(Result, BinaryTransferFunctionsKB::lshr(KB0, KB1));
      break;
    }
//   case LShrExact:
//     return "lshrexact";
    case Inst::AShr:
      // Synthesized constant cannot be zero.
      if (isReservedConst(I->Ops[1])) {
        if (KB0.Zero[KB0.getBitWidth() - 1])
          Result.Zero.setHighBits(2);
        if (KB0.One[KB0.getBitWidth() - 1])
          Result.One.setHighBits(2);
      }

      Result = getMostPreciseKnownBits(Result, BinaryTransferFunctionsKB::ashr(KB0, KB1));
      break;
//   case AShrExact:
//     return "ashrexact";
    case Inst::Select:
      if (KB0.One.getBoolValue())
        Result = KB1;
      else if (KB0.Zero.getBoolValue())
        Result = KB2;
      else
        Result = mergeKnownBits({KB1, KB2});
      break;
    case Inst::ZExt: {
      // below code copied from LLVM master. Directly use KnownBits::zext() when
      // we move to LLVM9
      unsigned OldBitWidth = KB0.getBitWidth();
      APInt NewZero = KB0.Zero.zext(I->Width);
      NewZero.setBitsFrom(OldBitWidth);
      Result.Zero = NewZero;
      Result.One = KB0.One.zext(I->Width);
      break;
    }
    case Inst::SExt:
      Result = KB0.sext(I->Width);
      break;
    case Inst::Trunc:
      Result = KB0.trunc(I->Width);
      break;
    case Inst::Eq: {
      Result = BinaryTransferFunctionsKB::eq(KB0, KB1);
      break;
    }
    case Inst::Ne:
      Result = BinaryTransferFunctionsKB::ne(KB0, KB1);
      break;
    case Inst::Ult:
      Result = BinaryTransferFunctionsKB::ult(KB0, KB1);
      break;
    case Inst::Slt:
      Result = BinaryTransferFunctionsKB::slt(KB0, KB1);
      break;
    case Inst::Ule:
      Result = BinaryTransferFunctionsKB::ule(KB0, KB1);
      break;
    case Inst::Sle:
      Result = BinaryTransferFunctionsKB::sle(KB0, KB1);
      break;
    case Inst::CtPop: {
      APInt val(KB0.getBitWidth(), KB0.countMaxPopulation());
      Result.Zero.setHighBits(KB0.getBitWidth() - val.getActiveBits());
      break;
    }
    case Inst::BSwap: {
      Result = KB0;
      Result.One = Result.One.byteSwap();
      Result.Zero = Result.Zero.byteSwap();
      break;
    }
    case Inst::BitReverse: {
      Result = KB0;
      Result.One = Result.One.reverseBits();
      Result.Zero = Result.Zero.reverseBits();
      break;
    }
    case Inst::Cttz: {
      APInt val(KB0.getBitWidth(), KB0.countMaxTrailingZeros());
      Result.Zero.setHighBits(KB0.getBitWidth() - val.getActiveBits());
      break;
    }
    case Inst::Ctlz: {
      APInt val(KB0.getBitWidth(), KB0.countMaxLeadingZeros());
      Result.Zero.setHighBits(KB0.getBitWidth() - val.getActiveBits());
      break;
    }
    case Inst::FShl: {
      auto NewKB0 = concatKnownBits(KB0, KB1);
      if (KB2.isConstant()) {
        auto Shift = KB2.getConstant().urem(I->Width);
        NewKB0.Zero = NewKB0.Zero.shl(Shift);
        NewKB0.One = NewKB0.One.shl(Shift);
        Result = NewKB0.trunc(I->Width);
      } else {
        llvm::KnownBits KBW(I->Width);
        KBW.One = I->Width;
        KBW.Zero = ~I->Width;
        auto NewKB1 = BinaryTransferFunctionsKB::urem(KB2, KBW);
        Result = BinaryTransferFunctionsKB::shl(NewKB0, NewKB1).trunc(I->Width);
      }
      break;
    }
    case Inst::FShr: {
      auto NewKB0 = concatKnownBits(KB0, KB1);
      if (KB2.isConstant()) {
        auto Shift = KB2.getConstant().urem(I->Width);
        NewKB0.Zero = NewKB0.Zero.lshr(Shift);
        NewKB0.One = NewKB0.One.lshr(Shift);
        Result = NewKB0.trunc(I->Width);
      } else {
        llvm::KnownBits KBW(I->Width);
        KBW.One = I->Width;
        KBW.Zero = ~I->Width;
        auto NewKB1 = BinaryTransferFunctionsKB::urem(KB2, KBW);
        Result = BinaryTransferFunctionsKB::lshr(NewKB0, NewKB1).trunc(I->Width);
      }
      break;
    }
    case souper::Inst::ExtractValue: {
      if (I->Ops[1]->Val == 0) {
        auto IOld = I;
        I = I->Ops[0]->Ops[0];
        switch (IOld->Ops[0]->K) {
          case souper::Inst::SAddWithOverflow:
          case souper::Inst::UAddWithOverflow:
            return BinaryTransferFunctionsKB::add(KB0, KB1);

          case souper::Inst::SSubWithOverflow:
          case souper::Inst::USubWithOverflow:
            return BinaryTransferFunctionsKB::sub(KB0, KB1);

          case souper::Inst::SMulWithOverflow:
          case souper::Inst::UMulWithOverflow:
            return BinaryTransferFunctionsKB::mul(KB0, KB1);
          default:
            llvm::report_fatal_error("Wrong operand in ExtractValue.");
        }
        I = IOld; // needed for caching
      }
      // returns TOP for the carry bit
    }

//   case ReservedConst:
//     return "reservedconst";
//   case ReservedInst:
//     return "reservedinst";
    default :
      break;
    }

    assert(!Result.hasConflict() && "Conflict in resulting KB!");

    KBCache.emplace(I, Result);
    return KBCache.at(I);
  }

#undef KB0
#undef KB1
#undef KB2

  llvm::KnownBits KnownBitsAnalysis::findKnownBitsUsingSolver(Inst *I,
                                                              Solver *S,
                                                              std::vector<InstMapping> &PCs) {
    BlockPCs BPCs;
    InstContext IC;
    KnownBits k(I->Width);
    S->knownBits(BPCs, PCs, I, k, IC);
    return k;
  }

#define CR0 findConstantRange(I->Ops[0], CI, UsePartialEval)
#define CR1 findConstantRange(I->Ops[1], CI, UsePartialEval)
#define CR2 findConstantRange(I->Ops[2], CI, UsePartialEval)

  bool ConstantRangeAnalysis::cacheHasValue(Inst *I) {
    if (CRCache.find(I) != CRCache.end())
      return true;

    if (I->K == Inst::Var && !I->Range.isFullSet()) {
      CRCache.emplace(I, I->Range);
      return true;
    }

    return false;
  }

  llvm::ConstantRange ConstantRangeAnalysis::findConstantRange(Inst *I,
                                                               ConcreteInterpreter &CI,
                                                               bool UsePartialEval) {
    llvm::ConstantRange Result(I->Width, /*isFullSet=*/true);

    if (cacheHasValue(I))
      return CRCache.at(I);

    if (UsePartialEval || I->K == Inst::Const) {
    EvalValue V = VAL(I);
    if (V.hasValue()) {
      CRCache.emplace(I, llvm::ConstantRange(V.getValue()));
      return CRCache.at(I);
    }
    }

    switch (I->K) {
    case Inst::Freeze: {
      Result = CR0;
      break;
    }
    case Inst::Const:
    case Inst::Var :
      if (isReservedConst(I))
        Result = llvm::ConstantRange(llvm::APInt(I->Width, 0)).inverse();
      break;
    case Inst::Trunc:
      Result = CR0.truncate(I->Width);
      break;
    case Inst::SExt:
      Result = CR0.signExtend(I->Width);
      break;
    case Inst::ZExt:
      Result = CR0.zeroExtend(I->Width);
      break;
    case souper::Inst::AddNUW :
    case souper::Inst::AddNW : // TODO: Rethink if these make sense
    case Inst::Add:
      Result = CR0.add(CR1);
      break;
    case Inst::AddNSW: {
      auto V1 = VAL(I->Ops[1]);
      if (V1.hasValue()) {
        Result = CR0.addWithNoWrap(V1.getValue(), OverflowingBinaryOperator::NoSignedWrap);
      }
      break;
    }
    case souper::Inst::SubNSW :
    case souper::Inst::SubNUW :
    case souper::Inst::SubNW : // TODO: Rethink if these make sense
    case Inst::Sub:
      Result = CR0.sub(CR1);
      break;
    case souper::Inst::MulNSW :
    case souper::Inst::MulNUW :
    case souper::Inst::MulNW : // TODO: Rethink if these make sense
    case Inst::Mul:
      Result = CR0.multiply(CR1);
      break;
    case Inst::And:
      Result = BinaryTransferFunctionsCR::binaryAnd(CR0, CR1);
      break;
    case Inst::Or:
      Result = BinaryTransferFunctionsCR::binaryOr(CR0, CR1);
      break;
    case souper::Inst::ShlNSW :
    case souper::Inst::ShlNUW :
    case souper::Inst::ShlNW : // TODO: Rethink if these make sense
    case Inst::Shl:
      Result = CR0.shl(CR1);
      break;
    case Inst::AShr:
      Result = CR0.ashr(CR1);
      break;
    case Inst::LShr:
      Result = CR0.lshr(CR1);
      break;
    case Inst::UDiv:
      Result = CR0.udiv(CR1);
      break;
    case Inst::Ctlz:
    case Inst::Cttz:
      // Synthesized constant cannot be zero.
      Result = llvm::ConstantRange(llvm::APInt(I->Width, 0),
                                   llvm::APInt(I->Width, isReservedConst(I->Ops[0]) ?
                                               I->Ops[0]->Width :
                                               (I->Ops[0]->Width + 1)));
      break;
    case Inst::CtPop:
      // Synthesized constant cannot be zero.
      Result = llvm::ConstantRange(llvm::APInt(I->Width, isReservedConst(I->Ops[0]) ? 1 : 0),
                                   llvm::APInt(I->Width, I->Ops[0]->Width + 1));
      break;
    case Inst::Phi: {
        auto Result = CR0;
        for (size_t i = 1; i < I->Ops.size(); ++i) {
          Result = Result.unionWith(findConstantRange(I->Ops[i], CI, UsePartialEval));
        }
      }
      break;
    case Inst::Select: {
        auto TVal = CR1, FVal = CR2;
        if (isReservedConst(I->Ops[1])) TVal = llvm::ConstantRange(I->Width, /*isFullSet=*/true);
        if (isReservedConst(I->Ops[2])) FVal = llvm::ConstantRange(I->Width, /*isFullSet=*/true);
        if (getSetSize(CR0) == 1) {
          if (CR0.contains(APInt(1, 1)))
            Result = TVal;
          else if (CR0.contains(APInt(1, 0)))
            Result = FVal;
        } else {
          Result = TVal.unionWith(FVal);
        }
      }
      break;
      //     case Inst::SDiv: {
      //       auto R0 = FindConstantRange(I->Ops[0], C);
      //       auto R1 = FindConstantRange(I->Ops[1], C);
      //       return R0.sdiv(R1); // unimplemented
      //     }
      // TODO: Xor pattern for not, truncs and extends, etc
    case souper::Inst::ExtractValue: {
      if (I->Ops[1]->Val == 0) {
        auto IOld = I;
        I = I->Ops[0]->Ops[0];
        switch (IOld->Ops[0]->K) {
          case souper::Inst::SAddWithOverflow:
          case souper::Inst::UAddWithOverflow:
            return CR0.add(CR1);

          case souper::Inst::SSubWithOverflow:
          case souper::Inst::USubWithOverflow:
            return CR0.sub(CR1);

          case souper::Inst::SMulWithOverflow:
          case souper::Inst::UMulWithOverflow:
            return CR0.multiply(CR1);
          default:
            llvm::errs() << Inst::getKindName(I->Ops[0]->K) << "\n";
            llvm::report_fatal_error("Wrong operand in ExtractValue.");
        }
        I = IOld; // needed for caching
      }
      // returns TOP for the carry bit
    }
    default:
      break;
    }

    CRCache.emplace(I, Result);
    return CRCache.at(I);
  }
#undef CR0
#undef CR1
#undef CR2
#undef VAL

  llvm::ConstantRange ConstantRangeAnalysis::findConstantRangeUsingSolver
    (Inst *I, Solver *S, std::vector<InstMapping> &PCs) {
    // FIXME implement this
    llvm::ConstantRange Result(I->Width, /*isFullSet=*/true);
    return Result;
  }
#define RB0 findRestrictedBits(I->Ops[0])
#define RB1 findRestrictedBits(I->Ops[1])
#define RB2 findRestrictedBits(I->Ops[2])
  llvm::APInt RestrictedBitsAnalysis::findRestrictedBits(souper::Inst *I) {
    llvm::APInt Result(I->Width, 0);
    llvm::APInt AllZeroes = Result;
    Result.setAllBits();
    if (RBCache.find(I) != RBCache.end()) {
      llvm::APInt CachedResult = RBCache[I];
      if (I->K == Inst::Var) {
        RBCache[I] = Result; // set to all ones after 'first' use
      }
      return CachedResult;
    }

    if (isReservedConst(I)) {
      // nop, all bits set
    } else if (I->K == Inst::Kind::Var) {
      RBCache[I] = Result;
      // ^ Restricts the variable
      Result = AllZeroes;
      // One variable can be considered unrestricted only once
      // TODO Pick a better strategy. This one chooses the DFS winner.
    } else switch (I->K) {

      case Inst::And:
      case Inst::Or:
        Result = RB0 | RB1;
        break;

      case Inst::Xor:
        Result = RB0 & RB1;
        break;

      case Inst::Eq:
      case Inst::Ne:
        Result = (RB0 & RB1) != 0;
        break;

      // bivalent if one of the input bits is bivalent
      // or the carry bit is bivalent
      case Inst::Add:
      case Inst::Sub:
        Result = RB0 & RB1;
        Result &= ~(~RB0 + ~RB1);
        break;

      case Inst::BitReverse:
        Result = RB0.reverseBits();
        break;

      case Inst::Freeze:
        Result = RB0;
        break;

      case Inst::Trunc:
        Result = RB0.trunc(I->Width);
        break;

      case Inst::BSwap:
        Result = RB0.byteSwap();
        break;

      case Inst::Select:
        Result = (RB0 == 0) ? (RB1 & RB2) : (RB1 | RB2);
        break;

      case Inst::Ule:
      case Inst::Sle:
      case Inst::Ult:
      case Inst::Slt:
        if (RB0 == 0 && RB1 == 0)
          Result = AllZeroes;
        break;

      case Inst::URem:
      case Inst::SRem:
        if (I->Width == 1) {
          Result = APInt(1, 1);
        } else {
          if (RB0 == 0 && RB1 == 0)
            Result = AllZeroes;
        }
        break;

      // Only unrestricted if both inputs are unrestricted
      // TODO Verify if N(S/U)?W variants fit in this category
      case Inst::Mul:
      case Inst::MulNSW:
      case Inst::MulNUW:
      case Inst::MulNW:
      case Inst::SDiv:
      case Inst::UDiv:
      case Inst::Shl:
      case Inst::LShr:
      case Inst::ShlNSW:
      case Inst::ShlNUW:
      case Inst::ShlNW:
      case Inst::AddNSW:
      case Inst::AddNUW:
      case Inst::AddNW:
      case Inst::SubNSW:
      case Inst::SubNUW:
      case Inst::SubNW:
        if (RB0 == 0 && RB1 == 0)
          Result = AllZeroes;
        break;

      // Only log2(Width) low bits can be unrestricted
      case Inst::Ctlz:
      case Inst::Cttz:
      case Inst::CtPop:
        if (RB0 == 0) {
          Result = AllZeroes;
          Result.setHighBits(I->Width - Log2_64(I->Width));
          // TODO Check for off by one issues
        }
        break;

      default:
        break; // TODO more precise transfer functions

    }
    if (I->K != Inst::Var) {
      RBCache[I] = Result;
    }
    return Result;
  }
#undef RB0
#undef RB1
#undef RB2

#define MDB0 findMustDemandedBits(I->Ops[0])
#define MDB1 findMustDemandedBits(I->Ops[1])
#define MDB2 findMustDemandedBits(I->Ops[2])
#define IVARS Uses.independentVars(I->Ops[0], I->Ops[1])

  InputVarInfo MustDemandedBitsAnalysis::findMustDemandedBitsImpl(souper::Inst *I) {
    if (Cache.find(I) != Cache.end()) {
      return Cache[I];
    }
    InputVarInfo Result;
    switch (I->K) {
      case Inst::Var:
        Result[I] = llvm::APInt::getAllOnesValue(I->Width);
        break;

      // Ops(#) where:
      // not exists C1, C2 forall x such that x # C1 == C2
      case Inst::Sub:
      case Inst::SubNSW:
      case Inst::SubNUW:
      case Inst::SubNW:
      case Inst::AddNSW:
      case Inst::AddNUW:
      case Inst::AddNW:
      case Inst::Add:
      case Inst::Xor: {
        auto A = MDB0, B = MDB1;
        auto IV = IVARS;

        for (auto &&P : A) {
          if (IV.find(P.first) == IV.end())
            continue;
          Result[P.first] = P.second;
        }

        for (auto &&P : B) {
          if (IV.find(P.first) == IV.end())
            continue;
          Result[P.first] = P.second;
        }
        break;
      }

      case Inst::And:
      case Inst::Or: {
        auto A = MDB0, B = MDB1;
        auto RB0 = RB.findRestrictedBits(I->Ops[0]);
        auto RB1 = RB.findRestrictedBits(I->Ops[1]);
        // Take bivalent bits of opposite operand to independent variables
        for (auto V : IVARS) {
          if (V->Width != I->Ops[0]->Width ||
              V->Width != I->Ops[1]->Width) {
            continue;
          }
          if (A.find(V) == A.end())
            A[V] = APInt::getNullValue(V->Width);
          if (B.find(V) == B.end())
            B[V] = APInt::getNullValue(V->Width);
          Result[V] = (~RB1 & A[V]) | (~RB0 & B[V]);
        }
      }
      default:
        break;
    }

    Cache[I] = Result;

    return Result;
  }

  InputVarInfo MustDemandedBitsAnalysis::findMustDemandedBits(souper::Inst *I) {
    InputVarInfo Result = findMustDemandedBitsImpl(I);

    // fill missing Result input variables with all zeros
    std::vector<Inst*> Vars;
    findVars(I, Vars);
    for (auto &var : Vars) {
      if (Result.find(var) == Result.end()) {
        Result[var] = APInt::getNullValue(var->Width);
      }
    }

    return Result;
  }

#undef MDB0
#undef MDB1
#undef MDB2

  InputVarInfo DontCareBitsAnalysis::findDontCareBits(souper::Inst *Root) {
    InputVarInfo Result;
    std::vector<Inst *> Inputs;
    findVars(Root, Inputs);

    for (auto V : Inputs) {
      Result[V] = llvm::APInt::getNullValue(V->Width);
    }

    return Result;
  }


/** python + z3 script as evidence for the following transfer functions
# unsat indicates one hole input is sufficient to make the output a hole(/any possible value)
# sat indicates both inputs have to be holes for that
**BEGIN SCRIPT
from z3 import *
s = Solver()
x, y, z = Consts("x y z", BitVecSort(16))
s.push(); s.add(ForAll(z, y != (x + z))); print("+", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x - z))); print("-", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x * z))); print("*", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x / z))); print("/", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x | z))); print("|", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x & z))); print("&", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x ^ z))); print("^", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != SRem(x, z))); print("srem", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != URem(x, z))); print("urem", s.check()); s.pop()
y = Const("r", BoolSort())
s.push(); s.add(ForAll(z, y != (x == z))); print("==", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x != z))); print("!=", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != ULE(x, z))); print("ule", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != ULT(x, z))); print("ult", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x <= z))); print("sle", s.check()); s.pop()
s.push(); s.add(ForAll(z, y != (x < z))); print("slt", s.check()); s.pop()
**END SCRIPT*/

  bool HoleAnalysis::findIfHole(souper::Inst *I) {
    if (Cache.find(I) != Cache.end()) {
      return Cache[I];
    }

    if (I->K == Inst::Hole || I->K == Inst::ReservedInst) {
      Cache[I] = true;
      return true;
    }
    bool op0isHole = false;
    bool op1isHole = false;
    bool op2isHole = false;
    bool hasHoleInput = false;
    bool allHoles = true;
    for (auto i = 0 ; i < I->Ops.size(); ++i) {
      bool isHole = findIfHole(I->Ops[i]);
      hasHoleInput |= isHole;
      if (i == 0 && isHole) {
        op0isHole = true;
      }
      if (i == 1 && isHole) {
        op1isHole = true;
      }
      if (i == 2 && isHole) {
        op2isHole = true;
      }
      allHoles &= isHole;
    }

    switch (I->K) {
      case Inst::Add:
      case Inst::Sub:
      case Inst::Eq:
      case Inst::Ne:
      case Inst::Xor:
      case Inst::BitReverse:
      case Inst::BSwap:
      case Inst::Freeze:
      case Inst::Trunc: {
        Cache[I] = hasHoleInput;
        return hasHoleInput;
      }
      case Inst::Shl:
      case Inst::LShr:
      case Inst::AShr: {
        Cache[I] = op1isHole;
        return op1isHole;
      }

      case Inst::Select : {
        auto Cond = op0isHole && (op1isHole || op2isHole);
        Cache[I] = Cond;
        return Cond;
      }

      // The NSW/NUW/NW variants for Add/Sub could be more precise.
      // I have not been able to prove that.
      // Conservatively, their place is here.
      case Inst::And:
      case Inst::Or:
      case Inst::AddNSW:
      case Inst::AddNUW:
      case Inst::AddNW:
      case Inst::SubNSW:
      case Inst::SubNUW:
      case Inst::SubNW:
      case Inst::Mul:
      case Inst::MulNSW:
      case Inst::MulNUW:
      case Inst::MulNW:
      case Inst::SDiv:
      case Inst::UDiv:
      case Inst::SRem:
      case Inst::URem:
      case Inst::Sle:
      case Inst::Slt:
      case Inst::Ule:
      case Inst::Ult:{
        Cache[I] = allHoles;
        return allHoles;
      }

      // It's sound to always return false, for the purpose we are using this.
      default: {
        Cache[I] = false;
        return false;
      }
    }
  }

  namespace BackwardsKnownBitsTF {

    llvm::KnownBits And(llvm::KnownBits R, llvm::KnownBits Op) {
//      auto KB0 = (R.Zero & R.One)  |
//                 (Op.One & R.Zero) |
//                 (Op.Zero & R.One) |
//                 (Op.Zero & Op.One);
//      auto KB1 = R.One | (Op.Zero & Op.One);
      // This is POSSIBLY wrong.
      // TODO exhaustive tester
      // TODO figure out correct boolean function
      llvm::KnownBits Other(R.getBitWidth());
      assert(!R.hasConflict() && !Op.hasConflict());
      for (size_t i = 0; i < R.getBitWidth(); ++i) {
        if (R.One[i]) {
          if (Op.Zero[i]) {
            // conflict
            Other.Zero.setBit(i);
            Other.One.setBit(i);
          } else {
            Other.One.setBit(i);
          }
        }
      }
      return Other;
    }

    llvm::KnownBits Or(llvm::KnownBits R, llvm::KnownBits Op) {
//      auto KB0 = R.Zero | (Op.Zero & Op.One);
//      auto KB1 = (R.Zero & R.One)  |
//           (Op.One & R.Zero) |
//           (Op.Zero & R.One) |
//           (Op.Zero & R.Zero)|
//           (Op.Zero & Op.One);
      // ^ This is wrong. Figure out the correct boolean functions
      llvm::KnownBits Other(R.getBitWidth());
      assert(!R.hasConflict() && !Op.hasConflict());
      for (size_t i = 0; i < R.getBitWidth(); ++i) {
        if (R.One[i]) {
          if (Op.Zero[i]) {
            Other.One.setBit(i);
          }
        }
        if (R.Zero[i]) {
          if (Op.One[i]) {
            // conflict
            Other.Zero.setBit(i);
            Other.One.setBit(i);
          } else {
            Other.Zero.setBit(i);
          }
        }
      }
      return Other;
    }

    llvm::KnownBits Add(llvm::KnownBits R, llvm::KnownBits Op) {
      return BinaryTransferFunctionsKB::sub(R, Op);
    }

    llvm::KnownBits Sub0(llvm::KnownBits Result, llvm::KnownBits Operand0) {
      return BinaryTransferFunctionsKB::sub(Operand0, Result);
    }

    llvm::KnownBits Sub1(llvm::KnownBits Result, llvm::KnownBits Operand1) {
      return BinaryTransferFunctionsKB::add(Operand1, Result);
    }

    llvm::KnownBits Xor(llvm::KnownBits R, llvm::KnownBits Op) {
      llvm::KnownBits Other(R.getBitWidth());
      assert(!R.hasConflict() && !Op.hasConflict());
      for (size_t i = 0; i < R.getBitWidth(); ++i) {
        if (R.One[i]) {
          if (Op.Zero[i]) {
            Other.One.setBit(i);
          }
          if (Op.One[i]) {
            Other.Zero.setBit(i);
          }
        }
        if (R.Zero[i]) {
          if (Op.One[i]) {
            Other.One.setBit(i);
          }
          if (Op.Zero[i]) {
            Other.Zero.setBit(i);
          }
        }
      }
      return Other;
    }
  }

  namespace ValueTF {
    llvm::APInt Add(llvm::APInt Result, llvm::APInt Operand) {
      return Result - Operand;
    }
    llvm::APInt Xor(llvm::APInt Result, llvm::APInt Operand) {
      return (Result | Operand) & ~(Result & Operand);
    }
    llvm::APInt Sub0(llvm::APInt Result, llvm::APInt Operand0) {
      return Operand0 - Result;
    }
    llvm::APInt Sub1(llvm::APInt Result, llvm::APInt Operand1) {
      return Operand1 + Result;
    }
  }
  using FV = ForcedValueAnalysis::Value;

#define IFV(Op, Opn) !R.hasConcrete() || !Opn.hasConcrete() ? \
    FV(BackwardsKnownBitsTF::Op(R.getKB(), Opn.getKB())) :    \
    FV(ValueTF::Op(R.Concrete(), Opn.Concrete()))

  FV get0(Inst::Kind K, FV R, FV Op0) {
    switch (K) {
      case Inst::Add : return IFV(Add, Op0);
      case Inst::Xor : return IFV(Xor, Op0);
      case Inst::Sub : return IFV(Sub0, Op0);
      case Inst::And : return FV(BackwardsKnownBitsTF::And(R.getKB(),
                                                       Op0.getKB()));
      case Inst::Or : return FV(BackwardsKnownBitsTF::Or(R.getKB(),
                                                       Op0.getKB()));
      default: return {};
    }
  }
  FV get1(Inst::Kind K, FV R, FV Op1) {
    switch (K) {
      case Inst::Add : return IFV(Add, Op1);
      case Inst::Xor : return IFV(Xor, Op1);
      case Inst::Sub : return IFV(Sub1, Op1);
      case Inst::And : return FV(BackwardsKnownBitsTF::And(R.getKB(),
                                                         Op1.getKB()));
      case Inst::Or : return FV(BackwardsKnownBitsTF::Or(R.getKB(),
                                                         Op1.getKB()));
      default: return {};
    }
  }
  FV getUnary(Inst::Kind K, FV R) {
    switch (K) {
      case Inst::Freeze : return R;
      case Inst::BSwap : return FV(R.Concrete().byteSwap());
      default: return {};
    }
  }
#undef IFV

  bool ForcedValueAnalysis::forceInst(souper::Inst *I, Value Result,
    ConcreteInterpreter &CI, Worklist &ToDo) {

    std::vector<EvalValue> OpValues;
    size_t Missing = 0;
    for (auto Op : I->Ops) {
      if (Op->nReservedConsts == 0 && Op->nHoles == 0) {
        // only evaluate when fully concrete
        OpValues.push_back(CI.evaluateInst(Op));
      } else {
        Missing++;
        OpValues.push_back(EvalValue());
      }
    }
    if (!Missing) { // All operands could be fully evaluated
      return false; // Subtree is fully concrete
    }

    if (I->Ops.size() == 1) {
      auto Inv = getUnary(I->K, Result);
      if (Inv.hasKB() && Inv.getKB().hasConflict()) {
        return true;
      }
      if (Inv.hasConcrete() || Inv.hasKB()) {
        if (addForcedValue(I->Ops[0], Inv, ToDo)) {
          return true;
        }
      }
    } else if (I->Ops.size() == 2) {
      if (OpValues[0].hasValue() && !OpValues[1].hasValue()) {
        auto Inv = get0(I->K, Result, FV(OpValues[0].getValue()));
        if (Inv.hasKB() && Inv.getKB().hasConflict()) {
          return true;
        }
        if (Inv.hasConcrete() || Inv.hasKB()) {
          if (addForcedValue(I->Ops[1], Inv, ToDo)) {
            return true;
          }
        }
      }
      if (OpValues[1].hasValue() && !OpValues[0].hasValue()) {
        auto Inv = get1(I->K, Result, FV(OpValues[1].getValue()));
        if (Inv.hasKB() && Inv.getKB().hasConflict()) {
          return true;
        }
        if (Inv.hasConcrete() || Inv.hasKB()) {
          if (addForcedValue(I->Ops[0], Inv, ToDo)) {
            return true;
          }
        }
      }
    } else {
      // TODO Ternary; some cases for select should be easy.
    }
    return false;
  }

  void ForcedValueAnalysis::countSymbolicInsts(Inst *I) {
    I->nReservedConsts = isReservedConst(I) ? 1 : 0;
    I->nHoles = isHole(I) ? 1 : 0;
    for (auto Op : I->Ops) {
      if (Op->nReservedConsts == -1 || Op->nHoles == -1) {
        countSymbolicInsts(Op);
      }
      I->nReservedConsts += Op->nReservedConsts;
      I->nHoles += Op->nHoles;
    }
  }

  bool ForcedValueAnalysis::force(llvm::APInt Result, ConcreteInterpreter &CI) {
    Worklist ToDo{{RHS, {Result}}};
    while (!ToDo.empty()) {
      auto &&[I, Val] = ToDo.back();
      ToDo.pop_back();
      if (forceInst(I, Val, CI, ToDo)) {
        return true;
      }
    }
    return false;
  }

  bool ForcedValueAnalysis::addForcedValue(Inst *I, Value V, Worklist &ToDo) {
//    ReplacementContext RC;
//    llvm::errs() << "Trying to force: \n";
//    RC.printInst(I, llvm::errs(), true);
//    V.print(llvm::errs());

    if (isReservedConst(I)) {
      if (conflict()) {
        return true;
      }
      for (auto &&Val : ForcedValues[I]) {
        if (Val.conflict(V)) {
//          llvm::errs() << "Failed to force: \n";
//          RC.printInst(I, llvm::errs(), true);
//          V.print(llvm::errs());
          return true;
        }
      }
      ForcedValues[I].push_back(V);
    } else {
      ToDo.push_back({I, V});
    }
    return false;
  }

}
