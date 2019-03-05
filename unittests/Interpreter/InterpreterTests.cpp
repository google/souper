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
      llvm_unreachable();
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

  KnownBits merge(KnownBits a, KnownBits b) {
    assert(a.getBitWidth() == b.getBitWidth() && "Can only merge KnownBits of same width.");

    KnownBits res(a.getBitWidth());
    for (unsigned i = 0; i < a.getBitWidth(); i++) {
      if (a.One[i] == b.One[i] && b.One[i])
	a.One.setBit(i);
      if (a.Zero[i] == b.Zero[i] && a.Zero[i])
	a.Zero.setBit(i);
    }
    return res;
  }

  KnownBits setLowest(KnownBits x) {
    for (int i = 0; i < x.getBitWidth(); i++) {
      if (!x.Zero[i] && !x.One[i]) {
	x.One.setBit(i);
	return x;
      }
    }
    llvm_unreachable();
  }

  KnownBits clearLowest(KnownBits x) {
    for (int i = 0; i < x.getBitWidth(); i++) {
      if (!x.Zero[i] && !x.One[i]) {
	x.Zero.setBit(i);
	return x;
      }
    }
    llvm_unreachable();
  }

  KnownBits bruteForce(KnownBits x, KnownBits y, Inst::Kind Pred) {
    if (!isConcrete(x))
      return merge(bruteForce(setLowest(x), y, Pred),
		   bruteForce(clearLowest(x), y, Pred));
    if (!isConcrete(y))
      return merge(bruteForce(x, setLowest(y), Pred),
		   bruteForce(x, clearLowest(y), Pred));
    auto xc = x.getConstant();
    auto yc = y.getConstant();

    KnownBits res(x.getBitWidth());
    bool rcInit = true;
    APInt rc(x.getBitWidth(), 0);
    switch (Pred) {
    case Inst::AddNUW:
    case Inst::AddNSW:
    case Inst::AddNW:
    case Inst::Add:
      rc = xc + yc;
      break;
    case Inst::SubNUW:
    case Inst::SubNSW:
    case Inst::SubNW:
    case Inst::Sub:
      rc = xc - yc;
      break;
    case Inst::Mul:
      rc = xc * yc;
      break;
    case Inst::UDiv:
      if (yc != 0)
	rc = xc.udiv(yc);
      else
	rcInit = false;
      break;
    case Inst::URem:
      if (yc != 0)
	rc = xc.urem(yc);
      else
	rcInit = false;
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
      rc = xc << yc;
      break;
    case Inst::LShr:
      if (yc.getLimitedValue() < res.getBitWidth())
	rc = xc.lshr(yc.getLimitedValue());
      else
	rcInit = false;
      break;
    case Inst::AShr:
      if (yc.getLimitedValue() < res.getBitWidth())
	rc = xc.ashr(yc.getLimitedValue());
      else
	rcInit = false;
      break;
    case Inst::Eq:
      res = KnownBits(1);
      if (xc.eq(yc))
	rc = APInt(1, 1);
      else
	rcInit = false;
      break;
    case Inst::Ne:
      res = KnownBits(1);
      if (xc.ne(yc))
	rc = APInt(1, 1);
      else
	rcInit = false;
      break;
    case Inst::Ult:
      res = KnownBits(1);
      if (xc.ult(yc))
	rc = APInt(1, 1);
      else
	rcInit = false;
      break;
    case Inst::Slt:
      res = KnownBits(1);
      if (xc.slt(yc))
	rc = APInt(1, 1);
      else
	rcInit = false;
      break;
    case Inst::Ule:
      res = KnownBits(1);
      if (xc.ule(yc))
	rc = APInt(1, 1);
      else
	rcInit = false;
      break;
    case Inst::Sle:
      res = KnownBits(1);
      if (xc.sle(yc))
	rc = APInt(1, 1);
      else
	rcInit = false;
      break;
    default:
      llvm_unreachable();
    }

    if (rcInit) {
      res.One = rc;
      res.Zero = ~rc;
    }
    return res;
  }

  bool testFn(Inst::Kind pred) {
    llvm::KnownBits x(WIDTH);
    do {
      llvm::KnownBits y(WIDTH);
      do {
	KnownBits Res1;

	switch(pred) {
	case Inst::AddNUW:
	case Inst::AddNW:
	case Inst::Add:
	  Res1 = BinaryTransferFunctionsKB::add(x, y);
	  break;
	case Inst::AddNSW:
	  Res1 = BinaryTransferFunctionsKB::addnsw(x, y);
	  break;
	case Inst::SubNUW:
	case Inst::SubNW:
	case Inst::Sub:
	  Res1 = BinaryTransferFunctionsKB::sub(x, y);
	  break;
	case Inst::SubNSW:
	  Res1 = BinaryTransferFunctionsKB::subnsw(x, y);
	  break;
	case Inst::Mul:
	  Res1 = BinaryTransferFunctionsKB::mul(x, y);
	  break;
	case Inst::UDiv:
	  Res1 = BinaryTransferFunctionsKB::udiv(x, y);
	  break;
	case Inst::URem:
	  Res1 = BinaryTransferFunctionsKB::urem(x, y);
	  break;
	case Inst::And:
	  Res1 = BinaryTransferFunctionsKB::and_(x, y);
	  break;
	case Inst::Or:
	  Res1 = BinaryTransferFunctionsKB::or_(x, y);
	  break;
	case Inst::Xor:
	  Res1 = BinaryTransferFunctionsKB::xor_(x, y);
	  break;
	case Inst::Shl:
	  Res1 = BinaryTransferFunctionsKB::shl(x, y);
	  break;
	case Inst::LShr:
	  Res1 = BinaryTransferFunctionsKB::lshr(x, y);
	  break;
	case Inst::AShr:
	  Res1 = BinaryTransferFunctionsKB::ashr(x, y);
	  break;
	case Inst::Eq:
	  Res1 = BinaryTransferFunctionsKB::eq(x, y);
	  break;
	case Inst::Ne:
	  Res1 = BinaryTransferFunctionsKB::ne(x, y);
	  break;
	case Inst::Ult:
	  Res1 = BinaryTransferFunctionsKB::ult(x, y);
	  break;
	case Inst::Slt:
	  Res1 = BinaryTransferFunctionsKB::slt(x, y);
	  break;
	case Inst::Ule:
	  Res1 = BinaryTransferFunctionsKB::ule(x, y);
	  break;
	case Inst::Sle:
	  Res1 = BinaryTransferFunctionsKB::sle(x, y);
	  break;
	default:
	  llvm_unreachable();
	}

	KnownBits Res2 = bruteForce(x, y, pred);
	if (Res1.getBitWidth() != Res2.getBitWidth()) {
	  llvm::errs() << "Expected and Given have unequal bitwidths - Expected: "
		       << Res2.getBitWidth() << ", Given: " << Res1.getBitWidth() << '\n';
	  return false;
	}
	if (Res1.hasConflict() || Res2.hasConflict()) {
	  llvm::errs() << "Expected or Given result has a conflict\n";
	  return false;
	}

	for (unsigned i = 0; i < Res1.getBitWidth(); i++) {
	  if ((Res1.Zero[i] && Res2.One[i]) || (Res1.One[i] && Res2.Zero[i])) {
	    std::cout << "Unsound!! " << Inst::getKindName(pred) << std::endl;
	    std::cout << knownBitsString(x) << ' ' << Inst::getKindName(pred)
		      << ' ' << knownBitsString(y) << std::endl;
	    std::cout << "Calculated: " << knownBitsString(Res1) << '\n';
	    std::cout << "Expected: " << knownBitsString(Res2) << '\n';
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

  souper::ValueCache C;
  auto KB = souper::findKnownBits(I1, C);
  ASSERT_EQ(KB.One, 5);
  ASSERT_EQ(KB.Zero, ~5);

  Inst *I2 = IC.getInst(Inst::Var, 64, {});
  Inst *I3 = IC.getConst(llvm::APInt(64, 0xFF));
  Inst *I4 = IC.getInst(Inst::And, 64, {I2, I3});
  KB = souper::findKnownBits(I4, C, /*PartialEval=*/false);
  ASSERT_EQ(KB.One, 0);
  ASSERT_EQ(KB.Zero, ~0xFF);

  Inst *I5 = IC.getInst(Inst::Or, 64, {I2, I1});
  KB = souper::findKnownBits(I5, C, /*PartialEval=*/false);
  ASSERT_EQ(KB.One, 5);
  ASSERT_EQ(KB.Zero, 0);

  Inst *I6 = IC.getInst(Inst::Shl, 64, {I2, I1});
  KB = souper::findKnownBits(I6, C, /*PartialEval=*/false);
  ASSERT_EQ(KB.One, 0);
  ASSERT_EQ(KB.Zero, 31);
}

TEST(InterpreterTests, ConstantRange) {
  InstContext IC;

  Inst *I1 = IC.getConst(llvm::APInt(64, 5));

  souper::ValueCache C;
  auto CR = souper::findConstantRange(I1, C, /*PartialEval=*/false);
  ASSERT_EQ(CR.getLower(), 5);
  ASSERT_EQ(CR.getUpper(), 6);

  Inst *I2 = IC.getInst(Inst::Var, 64, {});
  Inst *I3 = IC.getConst(llvm::APInt(64, 0xFF));
  Inst *I4 = IC.getInst(Inst::And, 64, {I2, I3});
  CR = souper::findConstantRange(I4, C, /*PartialEval=*/false);
  ASSERT_EQ(CR.getLower(), 0);
  ASSERT_EQ(CR.getUpper(), 0xFF + 1);

  Inst *I5 = IC.getInst(Inst::Add, 64, {I4, I1});
  CR = souper::findConstantRange(I5, C, /*PartialEval=*/false);
  ASSERT_EQ(CR.getLower(), 5);
  ASSERT_EQ(CR.getUpper(), 0xFF + 5 + 1);
}
