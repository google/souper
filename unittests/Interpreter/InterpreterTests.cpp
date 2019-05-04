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
#include "llvm/IR/ConstantRange.h"
#include "llvm/Support/raw_ostream.h"

#include "InterpreterInfra.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Inst/Inst.h"
#include "gtest/gtest.h"

#include <iostream>

using namespace llvm;
using namespace souper;

TEST(InterpreterTests, KBTransferFunctions) {
  KBTesting kbObj;
  ASSERT_TRUE(kbObj.testFn(Inst::Add));
  ASSERT_TRUE(kbObj.testFn(Inst::AddNSW));
  ASSERT_TRUE(kbObj.testFn(Inst::Sub));
  ASSERT_TRUE(kbObj.testFn(Inst::SubNSW));
  ASSERT_TRUE(kbObj.testFn(Inst::Mul));
  ASSERT_TRUE(kbObj.testFn(Inst::UDiv));
  ASSERT_TRUE(kbObj.testFn(Inst::URem));
  ASSERT_TRUE(kbObj.testFn(Inst::And));
  ASSERT_TRUE(kbObj.testFn(Inst::Or));
  ASSERT_TRUE(kbObj.testFn(Inst::Xor));
  ASSERT_TRUE(kbObj.testFn(Inst::Shl));
  ASSERT_TRUE(kbObj.testFn(Inst::LShr));
  ASSERT_TRUE(kbObj.testFn(Inst::AShr));
  ASSERT_TRUE(kbObj.testFn(Inst::Eq));
  ASSERT_TRUE(kbObj.testFn(Inst::Ne));
  ASSERT_TRUE(kbObj.testFn(Inst::Ult));
  ASSERT_TRUE(kbObj.testFn(Inst::Slt));
  ASSERT_TRUE(kbObj.testFn(Inst::Ule));
  ASSERT_TRUE(kbObj.testFn(Inst::Sle));
}

TEST(InterpreterTests, CRTransferFunctions) {
  CRTesting crObj;
  ASSERT_TRUE(crObj.testFn(Inst::And));
  ASSERT_TRUE(crObj.testFn(Inst::Or));
}

TEST(InterpreterTests, KBCRReduction) {
  ConstantRange CR(WIDTH, /*isFullSet=*/false);
  KnownBits KB(WIDTH);
  do {
    do {
      KnownBits CalculatedKB = KB;
      ConstantRange CalculatedCR = CR;
      improveKBCR(CalculatedKB, CalculatedCR);

      KnownBits ExhaustiveKB = KB;
      ConstantRange ExhaustiveCR = CR;
      TestingUtil::exhaustiveKBCRReduction(ExhaustiveKB, ExhaustiveCR);

      if (KnownBitsAnalysis::isConflictingKB(CalculatedKB, ExhaustiveKB)) {
	outs() << "Unsound!! CR KB reduction for KB\n";
	outs() << "Original KB: " << KnownBitsAnalysis::knownBitsString(KB) << "\n";
	outs() << "Original CR: " << CR << "\n";
	outs() << "CalculatedKB: " << KnownBitsAnalysis::knownBitsString(CalculatedKB) << '\n';
	outs() << "ExhaustiveKB: " << KnownBitsAnalysis::knownBitsString(ExhaustiveKB) << '\n';
	ASSERT_TRUE(false);
      }

      if (!CalculatedCR.contains(ExhaustiveCR)) {
	outs() << "Unsound!! CR KB reduction for CR\n";
	outs() << "Original KB: " << KnownBitsAnalysis::knownBitsString(KB) << "\n";
	outs() << "Original CR: " << CR << "\n";
	outs() << "CalculatedCR: " << CalculatedCR << '\n';
	outs() << "ExhaustiveCR: " << ExhaustiveCR << '\n';
	ASSERT_TRUE(false);
      }

      CR = CRTesting::nextCR(CR);
    } while(!CR.isEmptySet());
  } while(KBTesting::nextKB(KB));
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
