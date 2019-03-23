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

using namespace llvm;

namespace {

  APInt getUMin(const KnownBits &x) { return x.One; }

  APInt getUMax(const KnownBits &x) { return ~x.Zero; }

  bool isSignKnown(const KnownBits &x) {
    unsigned W = x.getBitWidth();
    return x.One[W - 1] || x.Zero[W - 1];
  }

  APInt getSMin(const KnownBits &x) {
    if (isSignKnown(x))
      return x.One;
    APInt Min = x.One;
    Min.setBit(x.getBitWidth() - 1);
    return Min;
  }

  APInt getSMax(const KnownBits &x) {
    if (isSignKnown(x))
      return ~x.Zero;
    APInt Max = ~x.Zero;
    Max.clearBit(x.getBitWidth() - 1);
    return Max;
  }

  KnownBits getMostPreciseKnownBits(KnownBits a, KnownBits b) {
    unsigned unknownCountA =
      a.getBitWidth() - (a.Zero.countPopulation() + a.One.countPopulation());
    unsigned unknownCountB =
      b.getBitWidth() - (b.Zero.countPopulation() + b.One.countPopulation());
    return unknownCountA < unknownCountB ? a : b;
  }
} // anonymous

namespace souper {

  namespace BinaryTransferFunctionsKB {
    llvm::KnownBits add(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/true, /*NSW=*/false,
                                               lhs, rhs);
    }

    llvm::KnownBits addnsw(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/true, /*NSW=*/true,
                                               lhs, rhs);
    }

    llvm::KnownBits sub(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/false, /*NSW=*/false,
                                               lhs, rhs);
    }

    llvm::KnownBits subnsw(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      return llvm::KnownBits::computeForAddSub(/*Add=*/false, /*NSW=*/true,
                                               lhs, rhs);
    }

    llvm::KnownBits mul(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(lhs.getBitWidth());

      // TODO: Below only takes into account leading and trailing zeros. Maybe
      // also do something with leading ones or trailing ones for improvement?
      auto trailingZeros0 = lhs.countMinTrailingZeros();
      auto trailingZeros1 = rhs.countMinTrailingZeros();
      Result.Zero.setLowBits(std::min(trailingZeros0 + trailingZeros1, lhs.getBitWidth()));

      // check for leading zeros
      auto lz0 = lhs.countMinLeadingZeros();
      auto lz1 = rhs.countMinLeadingZeros();
      auto confirmedLeadingZeros = lz0 + lz1 - 1;
      auto resultSize = lhs.getBitWidth() + rhs.getBitWidth() - 1;
      if (resultSize - confirmedLeadingZeros < lhs.getBitWidth())
        Result.Zero.setHighBits(lhs.getBitWidth() - (resultSize - confirmedLeadingZeros));

      // two numbers odd means reuslt is odd
      if (lhs.One[0] && rhs.One[0])
        Result.One.setLowBits(1);

      return Result;
    }

    llvm::KnownBits udiv(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(lhs.getBitWidth());
      const auto width = Result.getBitWidth();

      unsigned LeadZ = lhs.countMinLeadingZeros();
      unsigned RHSMaxLeadingZeros = rhs.countMaxLeadingZeros();
      if (RHSMaxLeadingZeros != width)
        LeadZ = std::min(width, LeadZ + width - RHSMaxLeadingZeros - 1);
      Result.Zero.setHighBits(LeadZ);
      return Result;
    }

    llvm::KnownBits urem(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(lhs.getBitWidth());
      const auto width = Result.getBitWidth();

      if (rhs.isConstant()) {
        auto RA = rhs.getConstant();
        if (RA.isPowerOf2()) {
          auto LowBits = (RA - 1);
          Result = lhs;
          Result.Zero |= ~LowBits;
          Result.One &= LowBits;
          return Result;
        }
      }

      unsigned Leaders =
        std::max(lhs.countMinLeadingZeros(), rhs.countMinLeadingZeros());
      Result.resetAll();
      Result.Zero.setHighBits(Leaders);
      return Result;
    }

    llvm::KnownBits and_(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      auto result = lhs;
      result.One &= rhs.One;
      result.Zero |= rhs.Zero;
      return result;
    }

    llvm::KnownBits or_(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      auto result = lhs;
      result.One |= rhs.One;
      result.Zero &= rhs.Zero;
      return result;
    }

    llvm::KnownBits xor_(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      auto result = lhs;
      llvm::APInt KnownZeroOut =
        (lhs.Zero & rhs.Zero) | (lhs.One & rhs.One);
      result.One = (lhs.Zero & rhs.One) | (lhs.One & rhs.Zero);
      result.Zero = std::move(KnownZeroOut);
      // ^ logic copied from LLVM ValueTracking.cpp
      return result;
    }

    llvm::KnownBits shl(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(lhs.getBitWidth());
      const auto width = Result.getBitWidth();

      auto Op0KB = lhs;
      if (rhs.isConstant()) {
        auto Val = rhs.getConstant().getLimitedValue();
        if (Val < 0 || Val >= width) {
          return Result;
        }

        Op0KB.One <<= Val;
        Op0KB.Zero <<= Val;
        Op0KB.Zero.setLowBits(Val);
        // setLowBits takes an unsigned int, so getLimitedValue is harmless
        return Op0KB;
      }

      unsigned minValue = rhs.One.getLimitedValue();
      Result.Zero.setLowBits(std::min(lhs.countMinTrailingZeros() + minValue, width));

      return Result;
    }

    llvm::KnownBits lshr(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(lhs.getBitWidth());
      const auto width = Result.getBitWidth();

      auto Op0KB = lhs;
      if (rhs.isConstant()) {
        auto Val = rhs.getConstant().getLimitedValue();
        if (Val < 0 || Val >= width) {
          return Result;
        }
        Op0KB.One.lshrInPlace(Val);
        Op0KB.Zero.lshrInPlace(Val);
        Op0KB.Zero.setHighBits(Val);
        // setHighBits takes an unsigned int, so getLimitedValue is harmless
        return Op0KB;
      }

      unsigned minValue = rhs.One.getLimitedValue();
      Result.Zero.setHighBits(std::min(minValue + lhs.countMinLeadingZeros(), width));

      return Result;
    }

    llvm::KnownBits ashr(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(lhs.getBitWidth());
      const auto width = Result.getBitWidth();

      unsigned minValue = rhs.One.getLimitedValue();
      if (lhs.One.isSignBitSet()) {
        // confirmed: sign bit = 1
        Result.One.setHighBits(std::min(lhs.countMinLeadingOnes() + minValue, width));
      } else if (lhs.Zero.isSignBitSet()) {
        // confirmed: sign bit = 0
        Result.Zero.setHighBits(std::min(lhs.countMinLeadingZeros() + minValue, width));
      }
      return Result;
    }

    llvm::KnownBits eq(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(1);
      if (lhs.isConstant() && rhs.isConstant() && (lhs.getConstant() == rhs.getConstant())) {
        Result.One.setBit(0);
        return Result;
      }
      if (((lhs.One & rhs.Zero) != 0) || ((lhs.Zero & rhs.One) != 0)) {
        Result.Zero.setBit(0);
        return Result;
      }
      return Result;
    }

    llvm::KnownBits ne(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(1);

      if (lhs.isConstant() && rhs.isConstant() && (lhs.getConstant() == rhs.getConstant()))
        Result.Zero.setBit(0);
      if (((lhs.One & rhs.Zero) != 0) || ((lhs.Zero & rhs.One) != 0))
        Result.One.setBit(0);
      return Result;
    }

    llvm::KnownBits ult(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(1);
      if (getUMax(lhs).ult(getUMin(rhs)))
        Result.One.setBit(0);
      if (getUMin(lhs).uge(getUMax(rhs)))
        Result.Zero.setBit(0);
      return Result;
    }

    llvm::KnownBits slt(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(1);
      if (getSMax(lhs).slt(getSMin(rhs)))
        Result.One.setBit(0);
      if (getSMin(lhs).sge(getSMax(rhs)))
        Result.Zero.setBit(0);
      return Result;
    }

    llvm::KnownBits ule(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(1);
      if (getUMax(lhs).ule(getUMin(rhs)))
        Result.One.setBit(0);
      if (getUMin(lhs).ugt(getUMax(rhs)))
        Result.Zero.setBit(0);
      return Result;
    }

    llvm::KnownBits sle(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs) {
      llvm::KnownBits Result(1);
      if (getSMax(lhs).sle(getSMin(rhs)))
        Result.One.setBit(0);
      if (getSMin(lhs).sgt(getSMax(rhs)))
        Result.Zero.setBit(0);
      return Result;
    }
  }

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

  bool isConcrete(Inst *I, bool ConsiderConsts, bool ConsiderHoles) {
    return !hasGivenInst(I, [ConsiderConsts, ConsiderHoles](Inst* instr) {
      if (ConsiderConsts && isReservedConst(instr))
        return true;
      if (ConsiderHoles && isReservedInst(instr))
        return true;
      return false;
    });
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

    EvalValue RootVal = VAL(I);
    if (RootVal.hasValue()) {
      Result.One = RootVal.getValue();
      Result.Zero = ~RootVal.getValue();
      return Result;
    }

    for (auto Op : I->Ops) {
      if (findKnownBits(Op, C, PartialEval).hasConflict()) {
        assert(false && "Conflict KB");
      }
    }

    switch(I->K) {
//   case Phi:
//     return "phi";
    case Inst::AddNUW :
    case Inst::AddNW :
    case Inst::Add:
      return BinaryTransferFunctionsKB::add(KB0, KB1);
    case Inst::AddNSW:
      return BinaryTransferFunctionsKB::addnsw(KB0, KB1);
    case Inst::SubNUW :
    case Inst::SubNW :
    case Inst::Sub:
      return BinaryTransferFunctionsKB::sub(KB0, KB1);
    case Inst::SubNSW:
      return BinaryTransferFunctionsKB::subnsw(KB0, KB1);
    case Inst::Mul:
      return BinaryTransferFunctionsKB::mul(KB0, KB1);
//   case MulNSW:
//     return "mulnsw";
//   case MulNUW:
//     return "mulnuw";
//   case MulNW:
//     return "mulnw";
    case Inst::UDiv:
      return BinaryTransferFunctionsKB::udiv(KB0, KB1);
//   case SDiv:
//     return "sdiv";
//   case UDivExact:
//     return "udivexact";
//   case SDivExact:
//     return "sdivexact";
    case Inst::URem:
      return BinaryTransferFunctionsKB::urem(KB0, KB1);
//   case SRem:
//     return "srem";
    case Inst::And :
      return BinaryTransferFunctionsKB::and_(KB0, KB1);
    case Inst::Or :
      return BinaryTransferFunctionsKB::or_(KB0, KB1);
    case Inst::Xor :
      return BinaryTransferFunctionsKB::xor_(KB0, KB1);
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
      if (isReservedConst(I->Ops[1]))
        Result.Zero.setLowBits(1);
      return getMostPreciseKnownBits(Result, BinaryTransferFunctionsKB::shl(KB0, KB1));
    }
    case Inst::LShr : {
      if (isReservedConst(I->Ops[1]))
        Result.Zero.setHighBits(1);
      return getMostPreciseKnownBits(Result, BinaryTransferFunctionsKB::lshr(KB0, KB1));
    }
//   case LShrExact:
//     return "lshrexact";
    case Inst::AShr:
      if (isReservedConst(I->Ops[1])) {
        if (KB0.Zero[KB0.getBitWidth() - 1])
          Result.Zero.setHighBits(2);
        if (KB0.One[KB0.getBitWidth() - 1])
          Result.One.setHighBits(2);
      }

      return getMostPreciseKnownBits(Result, BinaryTransferFunctionsKB::ashr(KB0, KB1));
//   case AShrExact:
//     return "ashrexact";
//   case Select:
//     return "select";
    case Inst::ZExt:
      return KB0.zext(I->Width);
    case Inst::SExt:
      return KB0.sext(I->Width);
    case Inst::Trunc:
      return KB0.trunc(I->Width);
    case Inst::Eq: {
      // Below implementation, because it contains isReservedConst, is
      // difficult to put inside BinaryTransferFunctionsKB but it's able to
      // prune more stuff; so, let's keep both
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

      // Constants are never equal to 0
      if (Other.Zero.isAllOnesValue()) {
        Result.Zero.setBit(0);
        return Result;
      } else {
        return Result;
      }

      // Fallback to our tested implmentation
      return BinaryTransferFunctionsKB::eq(KB0, KB1);
    }
    case Inst::Ne:
      return BinaryTransferFunctionsKB::ne(KB0, KB1);
    case Inst::Ult:
      return BinaryTransferFunctionsKB::ult(KB0, KB1);
    case Inst::Slt:
      return BinaryTransferFunctionsKB::slt(KB0, KB1);
    case Inst::Ule:
      return BinaryTransferFunctionsKB::ule(KB0, KB1);
    case Inst::Sle:
      return BinaryTransferFunctionsKB::sle(KB0, KB1);
    case Inst::CtPop: {
      int activeBits = std::ceil(std::log2(KB0.countMaxPopulation()));
      Result.Zero.setHighBits(KB0.getBitWidth() - activeBits);
      return Result;
    }
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
    case Inst::Cttz: {
      int activeBits = std::ceil(std::log2(KB0.countMaxTrailingZeros()));
      Result.Zero.setHighBits(KB0.getBitWidth() - activeBits);
      return Result;
    }
    case Inst::Ctlz: {
      int activeBits = std::ceil(std::log2(KB0.countMaxLeadingZeros()));
      Result.Zero.setHighBits(KB0.getBitWidth() - activeBits);
      return Result;
    }
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

  llvm::KnownBits findKnownBitsUsingSolver(Inst *I, Solver *S, std::vector<InstMapping> &PCs) {
    BlockPCs BPCs;
    InstContext IC;
    return S->findKnownBitsUsingSolver(BPCs, PCs, I, IC);
  }

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
    case Inst::Trunc:
      return CR0.truncate(I->Width);
    case Inst::SExt:
      return CR0.signExtend(I->Width);
    case Inst::ZExt:
      return CR0.zeroExtend(I->Width);
    case souper::Inst::AddNUW :
    case souper::Inst::AddNW : // TODO: Rethink if these make sense
    case Inst::Add:
      return CR0.add(CR1);
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
    case Inst::Sub:
      return CR0.sub(CR1);
    case souper::Inst::MulNSW :
    case souper::Inst::MulNUW :
    case souper::Inst::MulNW : // TODO: Rethink if these make sense
    case Inst::Mul:
      return CR0.multiply(CR1);
    case Inst::And:
      return CR0.binaryAnd(CR1);
    case Inst::Or:
      return CR0.binaryOr(CR1);
    case souper::Inst::ShlNSW :
    case souper::Inst::ShlNUW :
    case souper::Inst::ShlNW : // TODO: Rethink if these make sense
    case Inst::Shl:
      return CR0.shl(CR1);
    case Inst::AShr:
      return CR0.ashr(CR1);
    case Inst::LShr:
      return CR0.lshr(CR1);
    case Inst::UDiv:
      return CR0.udiv(CR1);
    case Inst::Ctlz:
    case Inst::Cttz:
    case Inst::CtPop:
      return llvm::ConstantRange(llvm::APInt(I->Width, 0),
                                 llvm::APInt(I->Width, I->Ops[0]->Width + 1));
    case Inst::Select:
      return CR1.unionWith(CR2);
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

  llvm::ConstantRange findConstantRangeUsingSolver(Inst *I, Solver *S, std::vector<InstMapping> &PCs) {
    // FIXME implement this
    llvm::ConstantRange Result(I->Width);
    return Result;
  }
}
