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

#include "llvm/Support/KnownBits.h"
#include "llvm/Support/raw_ostream.h"

#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Inst/Inst.h"
#include "gtest/gtest.h"

#include <iostream>

using namespace llvm;
using namespace souper;

// width used for transfer function tests
constexpr int WIDTH = 4;

namespace {

  struct EvalValueKB : public EvalValue {
    KnownBits ValueKB;

    EvalValueKB(KnownBits Val) {
      K = ValueKind::Val;
      ValueKB = Val;
    }

    EvalValueKB(EvalValue &Val) : EvalValue(Val) {
      if (hasValue()) {
        ValueKB.One = Val.getValue();
        ValueKB.Zero = ~Val.getValue();
      }
    }

    KnownBits getValueKB() {
      if (K != ValueKind::Val) {
        llvm::errs() << "EvalValueKB: KnownBits not initialized.\n";
        llvm::report_fatal_error("exiting");
      }

      return ValueKB;
    }
  };


  bool nextKB(llvm::KnownBits &x) {
    for (int i = 0; i < x.getBitWidth(); i++) {
      if (!x.Zero[i] && !x.One[i]) {
        x.Zero.setBit(i);
        return true;
      }
      if (x.Zero[i] && !x.One[i]) {
        x.Zero.clearBit(i);
        x.One.setBit(i);
        return true;
      }
      if (!x.Zero[i] && x.One[i]) {
        x.Zero.clearBit(i);
        x.One.clearBit(i);
        continue;
      }
      // gtest doesn't allow putting fatal failures in non-void returning
      // functions;
      report_fatal_error("faulty nextKB!");
    }
    return false;
  }

  std::string knownBitsString(llvm::KnownBits KB) {
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

  bool isConcrete(KnownBits x) { return (x.Zero | x.One).isAllOnesValue(); }

  EvalValueKB merge(EvalValueKB a, EvalValueKB b) {
    if (!a.hasValue())
      return b;
    if (!b.hasValue())
      return a;

    // FIXME: Handle other ValueKind types. how?
    assert(a.hasValue() && b.hasValue());
    auto aVal = a.getValueKB();
    auto bVal = b.getValueKB();
    assert(aVal.getBitWidth() == bVal.getBitWidth() && "Can only merge KnownBits of same width.");

    KnownBits res(aVal.getBitWidth());
    for (unsigned i = 0; i < aVal.getBitWidth(); i++) {
      if (aVal.One[i] == bVal.One[i] && bVal.One[i])
        res.One.setBit(i);
      if (aVal.Zero[i] == bVal.Zero[i] && aVal.Zero[i])
        res.Zero.setBit(i);
    }

    return EvalValueKB(res);
  }

  KnownBits setLowest(KnownBits x) {
    for (int i = 0; i < x.getBitWidth(); i++) {
      if (!x.Zero[i] && !x.One[i]) {
        x.One.setBit(i);
        return x;
      }
    }
    report_fatal_error("faulty setLowest!");
  }

  KnownBits clearLowest(KnownBits x) {
    for (int i = 0; i < x.getBitWidth(); i++) {
      if (!x.Zero[i] && !x.One[i]) {
        x.Zero.setBit(i);
        return x;
      }
    }
    report_fatal_error("faulty clearLowest!");
  }

  EvalValueKB bruteForce(KnownBits x, KnownBits y, Inst::Kind Pred) {
    if (!isConcrete(x))
      return merge(bruteForce(setLowest(x), y, Pred),
                   bruteForce(clearLowest(x), y, Pred));
    if (!isConcrete(y))
      return merge(bruteForce(x, setLowest(y), Pred),
                   bruteForce(x, clearLowest(y), Pred));
    auto xc = x.getConstant();
    auto yc = y.getConstant();

    EvalValue res;
    bool rcInit = true;
    APInt rc(x.getBitWidth(), 0);
    switch (Pred) {
    case Inst::AddNUW:
      res = evaluateAddNUW(xc, yc);
      break;
    case Inst::AddNW:
      res = evaluateAddNW(xc, yc);
      break;
    case Inst::AddNSW:
      res = evaluateAddNSW(xc, yc);
      break;
    case Inst::Add:
      res = EvalValue(xc + yc);
      break;
    case Inst::SubNUW:
      res = evaluateSubNUW(xc, yc);
      break;
    case Inst::SubNSW:
      res = evaluateSubNSW(xc, yc);
      break;
    case Inst::SubNW:
      res = evaluateSubNW(xc, yc);
      break;
    case Inst::Sub:
      res = EvalValue(xc - yc);
      break;
    case Inst::Mul:
      res = EvalValue(xc * yc);
      break;
    case Inst::UDiv:
      res = evaluateUDiv(xc, yc);
      break;
    case Inst::URem:
      res = evaluateURem(xc, yc);
      break;
    case Inst::And:
      rc = xc & yc;
      break;
    case Inst::Or:
      rc = xc | yc;
      break;
    case Inst::Xor:
      rc = xc ^ yc;
      break;
    case Inst::Shl:
      res = evaluateShl(xc, yc);
      break;
    case Inst::LShr:
      res = evaluateLShr(xc, yc);
      break;
    case Inst::AShr:
      res = evaluateAShr(xc, yc);
      break;
    case Inst::Eq:
      rc = xc.eq(yc) ? APInt(1, 1) : APInt(1, 0);
      res = EvalValue(rc);
      break;
    case Inst::Ne:
      rc = xc.ne(yc) ? APInt(1, 1) : APInt(1, 0);
      res = EvalValue(rc);
      break;
    case Inst::Ult:
      rc = xc.ult(yc) ? APInt(1, 1) : APInt(1, 0);
      res = EvalValue(rc);
      break;
    case Inst::Slt:
      rc = xc.slt(yc) ? APInt(1, 1) : APInt(1, 0);
      res = EvalValue(rc);
      break;
    case Inst::Ule:
      rc = xc.ule(yc) ? APInt(1, 1) : APInt(1, 0);
      res = EvalValue(rc);
      break;
    case Inst::Sle:
      rc = xc.sle(yc) ? APInt(1, 1) : APInt(1, 0);
      res = EvalValue(rc);
      break;
    default:
      report_fatal_error("unhandled case in bruteForce!");
    }

    return res;
  }

  bool testFn(Inst::Kind pred) {
    llvm::KnownBits x(WIDTH);
    do {
      llvm::KnownBits y(WIDTH);
      do {
        KnownBits Calculated;
        switch(pred) {
        case Inst::AddNUW:
        case Inst::AddNW:
        case Inst::Add:
          Calculated = BinaryTransferFunctionsKB::add(x, y);
          break;
        case Inst::AddNSW:
          Calculated = BinaryTransferFunctionsKB::addnsw(x, y);
          break;
        case Inst::SubNUW:
        case Inst::SubNW:
        case Inst::Sub:
          Calculated = BinaryTransferFunctionsKB::sub(x, y);
          break;
        case Inst::SubNSW:
          Calculated = BinaryTransferFunctionsKB::subnsw(x, y);
          break;
        case Inst::Mul:
          Calculated = BinaryTransferFunctionsKB::mul(x, y);
          break;
        case Inst::UDiv:
          Calculated = BinaryTransferFunctionsKB::udiv(x, y);
          break;
        case Inst::URem:
          Calculated = BinaryTransferFunctionsKB::urem(x, y);
          break;
        case Inst::And:
          Calculated = BinaryTransferFunctionsKB::and_(x, y);
          break;
        case Inst::Or:
          Calculated = BinaryTransferFunctionsKB::or_(x, y);
          break;
        case Inst::Xor:
          Calculated = BinaryTransferFunctionsKB::xor_(x, y);
          break;
        case Inst::Shl:
          Calculated = BinaryTransferFunctionsKB::shl(x, y);
          break;
        case Inst::LShr:
          Calculated = BinaryTransferFunctionsKB::lshr(x, y);
          break;
        case Inst::AShr:
          Calculated = BinaryTransferFunctionsKB::ashr(x, y);
          break;
        case Inst::Eq:
          Calculated = BinaryTransferFunctionsKB::eq(x, y);
          break;
        case Inst::Ne:
          Calculated = BinaryTransferFunctionsKB::ne(x, y);
          break;
        case Inst::Ult:
          Calculated = BinaryTransferFunctionsKB::ult(x, y);
          break;
        case Inst::Slt:
          Calculated = BinaryTransferFunctionsKB::slt(x, y);
          break;
        case Inst::Ule:
          Calculated = BinaryTransferFunctionsKB::ule(x, y);
          break;
        case Inst::Sle:
          Calculated = BinaryTransferFunctionsKB::sle(x, y);
          break;
        default:
          report_fatal_error("unhandled case in testFn!");
        }

        EvalValueKB Expected = bruteForce(x, y, pred);
        // expected value is poison/ub; so let binary transfer functions do
        // whatever they want without complaining
        if (!Expected.hasValue())
          continue;

        if (Calculated.getBitWidth() != Expected.ValueKB.getBitWidth()) {
          llvm::errs() << "Expected and Given have unequal bitwidths - Expected: "
                       << Expected.ValueKB.getBitWidth() << ", Given: " << Calculated.getBitWidth() << '\n';
          return false;
        }
        if (Calculated.hasConflict() || Expected.ValueKB.hasConflict()) {
          llvm::errs() << "Expected or Given result has a conflict\n";
          return false;
        }

        for (unsigned i = 0; i < Calculated.getBitWidth(); i++) {
          if ((Calculated.Zero[i] && Expected.ValueKB.One[i]) ||
              (Calculated.One[i] && Expected.ValueKB.Zero[i]) ||
              (Calculated.One[i] && !Expected.ValueKB.One[i]) ||
              (Calculated.Zero[i] && !Expected.ValueKB.Zero[i])) {
            std::cout << "Unsound!! " << Inst::getKindName(pred) << std::endl;
            std::cout << knownBitsString(x) << ' ' << Inst::getKindName(pred)
                      << ' ' << knownBitsString(y) << std::endl;
            std::cout << "Calculated: " << knownBitsString(Calculated) << '\n';
            std::cout << "Expected: " << knownBitsString(Expected.ValueKB) << '\n';
            return false;
          }
        }

      } while(nextKB(y));
    } while(nextKB(x));

    return true;
  }
} // anon

TEST(InterpreterTests, KBTransferFunctions) {
  ASSERT_TRUE(testFn(Inst::Add));
  ASSERT_TRUE(testFn(Inst::AddNSW));
  ASSERT_TRUE(testFn(Inst::Sub));
  ASSERT_TRUE(testFn(Inst::SubNSW));
  ASSERT_TRUE(testFn(Inst::Mul));
  ASSERT_TRUE(testFn(Inst::UDiv));
  ASSERT_TRUE(testFn(Inst::URem));
  ASSERT_TRUE(testFn(Inst::And));
  ASSERT_TRUE(testFn(Inst::Or));
  ASSERT_TRUE(testFn(Inst::Xor));
  ASSERT_TRUE(testFn(Inst::Shl));
  ASSERT_TRUE(testFn(Inst::LShr));
  ASSERT_TRUE(testFn(Inst::AShr));
  ASSERT_TRUE(testFn(Inst::Eq));
  ASSERT_TRUE(testFn(Inst::Ne));
  ASSERT_TRUE(testFn(Inst::Ult));
  ASSERT_TRUE(testFn(Inst::Slt));
  ASSERT_TRUE(testFn(Inst::Ule));
  ASSERT_TRUE(testFn(Inst::Sle));
}

TEST(InterpreterTests, KnownBits) {
  InstContext IC;

  Inst *I1 = IC.getConst(llvm::APInt(64, 5));

  souper::ConcreteInterpreter CI;
  auto KB = souper::KnownBitsAnalysis().findKnownBits(I1, CI);
  ASSERT_EQ(KB.One, 5);
  ASSERT_EQ(KB.Zero, ~5);

  Inst *I2 = IC.getInst(Inst::ReservedConst, 64, {});
  Inst *I3 = IC.getConst(llvm::APInt(64, 0xFF));
  Inst *I4 = IC.getInst(Inst::And, 64, {I2, I3});
  KB = souper::KnownBitsAnalysis().findKnownBits(I4, CI);
  ASSERT_EQ(KB.One, 0);
  ASSERT_EQ(KB.Zero, ~0xFF);

  Inst *I5 = IC.getInst(Inst::Or, 64, {I2, I1});
  KB = souper::KnownBitsAnalysis().findKnownBits(I5, CI);
  ASSERT_EQ(KB.One, 5);
  ASSERT_EQ(KB.Zero, 0);

  Inst *I6 = IC.getInst(Inst::Shl, 64, {I2, I1});
  KB = souper::KnownBitsAnalysis().findKnownBits(I6, CI);
  ASSERT_EQ(KB.One, 0);
  ASSERT_EQ(KB.Zero, 31);
}

TEST(InterpreterTests, ConstantRange) {
  InstContext IC;

  Inst *I1 = IC.getConst(llvm::APInt(64, 5));

  souper::ConcreteInterpreter CI;
  auto CR = souper::ConstantRangeAnalysis().findConstantRange(I1, CI);
  ASSERT_EQ(CR.getLower(), 5);
  ASSERT_EQ(CR.getUpper(), 6);

  Inst *I2 = IC.getInst(Inst::ReservedConst, 64, {});
  Inst *I3 = IC.getConst(llvm::APInt(64, 0xFF));
  Inst *I4 = IC.getInst(Inst::And, 64, {I2, I3});
  CR = souper::ConstantRangeAnalysis().findConstantRange(I4, CI);
  ASSERT_EQ(CR.getLower(), 0);
  ASSERT_EQ(CR.getUpper(), 0xFF + 1);

  Inst *I5 = IC.getInst(Inst::Add, 64, {I4, I1});
  CR = souper::ConstantRangeAnalysis().findConstantRange(I5, CI);
  ASSERT_EQ(CR.getLower(), 5);
  ASSERT_EQ(CR.getUpper(), 0xFF + 5 + 1);
}

// Checks that ConcreteInterpreter only caches during construction, otherwise not
TEST(InterpreterTests, ConcreteCache) {
  InstContext IC;

  Inst *I1 = IC.getConst(llvm::APInt(8, 0xFF));
  Inst *I2 = IC.getInst(Inst::Var, 8, {});
  Inst *I3 = IC.getInst(Inst::Or, 8, {I1, I2});

  ValueCache InputValues = {{I2, APInt(8, 0x00)}};
  souper::ConcreteInterpreter CI(InputValues);
  auto Val = CI.evaluateInst(I3);
  ASSERT_TRUE(Val.hasValue());

  ASSERT_EQ(Val.getValue(), APInt(8, 0xFF));

  // We want to ensure that evaluateInst call is *really* being evaluated
  // instead of just returning the result from the cache; so let's change what
  // I1 was pointing to, to see that.
  *I1 = *IC.getConst(llvm::APInt(8, 0x0F));
  Val = CI.evaluateInst(I3);
  ASSERT_TRUE(Val.hasValue());

  // We would have got 0xFF if evaluateInst had returned result from cache.
  ASSERT_EQ(Val.getValue(), APInt(8, 0x0F, true));
}
