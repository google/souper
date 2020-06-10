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

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/Support/raw_ostream.h"

#include "InterpreterInfra.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Inst/Inst.h"
#include "gtest/gtest.h"

#include <iostream>

namespace {
  static llvm::cl::opt<bool> CheckRBPrecision("check-rb-precision",
  llvm::cl::desc("Print precision results from restricted bits exhaustive testing"),
  llvm::cl::init(false));

}

int DebugLevel;

using namespace llvm;
using namespace souper;

TEST(InterpreterTests, KBTransferFunctions) {
  for (int WIDTH = 1; WIDTH <= MAX_WIDTH; ++WIDTH) {
    KBTesting kbObj(WIDTH);
    ASSERT_TRUE(kbObj.testFn(Inst::Add));
    ASSERT_TRUE(kbObj.testFn(Inst::AddNSW));
    ASSERT_TRUE(kbObj.testFn(Inst::AddNUW));
    ASSERT_TRUE(kbObj.testFn(Inst::AddNW));
    ASSERT_TRUE(kbObj.testFn(Inst::Sub));
    ASSERT_TRUE(kbObj.testFn(Inst::SubNSW));
    ASSERT_TRUE(kbObj.testFn(Inst::SubNUW));
    ASSERT_TRUE(kbObj.testFn(Inst::SubNW));
    ASSERT_TRUE(kbObj.testFn(Inst::Mul));
    ASSERT_TRUE(kbObj.testFn(Inst::MulNSW));
    ASSERT_TRUE(kbObj.testFn(Inst::MulNUW));
    ASSERT_TRUE(kbObj.testFn(Inst::MulNW));
    ASSERT_TRUE(kbObj.testFn(Inst::UDiv));
    ASSERT_TRUE(kbObj.testFn(Inst::SDiv));
    ASSERT_TRUE(kbObj.testFn(Inst::UDivExact));
    ASSERT_TRUE(kbObj.testFn(Inst::SDivExact));
    ASSERT_TRUE(kbObj.testFn(Inst::URem));
    ASSERT_TRUE(kbObj.testFn(Inst::SRem));
    ASSERT_TRUE(kbObj.testFn(Inst::And));
    ASSERT_TRUE(kbObj.testFn(Inst::Or));
    ASSERT_TRUE(kbObj.testFn(Inst::Xor));
    ASSERT_TRUE(kbObj.testFn(Inst::Shl));
    ASSERT_TRUE(kbObj.testFn(Inst::ShlNSW));
    ASSERT_TRUE(kbObj.testFn(Inst::ShlNUW));
    ASSERT_TRUE(kbObj.testFn(Inst::ShlNW));
    ASSERT_TRUE(kbObj.testFn(Inst::LShr));
    ASSERT_TRUE(kbObj.testFn(Inst::LShrExact));
    ASSERT_TRUE(kbObj.testFn(Inst::AShr));
    ASSERT_TRUE(kbObj.testFn(Inst::AShrExact));
    ASSERT_TRUE(kbObj.testFn(Inst::Eq));
    ASSERT_TRUE(kbObj.testFn(Inst::Ne));
    ASSERT_TRUE(kbObj.testFn(Inst::Ult));
    ASSERT_TRUE(kbObj.testFn(Inst::Slt));
    ASSERT_TRUE(kbObj.testFn(Inst::Ule));
    ASSERT_TRUE(kbObj.testFn(Inst::Sle));
    ASSERT_TRUE(kbObj.testTernaryFn(Inst::Select, 1, WIDTH, WIDTH));
    ASSERT_TRUE(kbObj.testTernaryFn(Inst::FShl, WIDTH, WIDTH, WIDTH));
    ASSERT_TRUE(kbObj.testTernaryFn(Inst::FShr, WIDTH, WIDTH, WIDTH));
  }
}

TEST(InterpreterTests, CRTransferFunctions) {
  for (int WIDTH = 1; WIDTH <= MAX_WIDTH; ++WIDTH) {
    CRTesting crObj(WIDTH);
    ASSERT_TRUE(crObj.testFn(Inst::And));
    ASSERT_TRUE(crObj.testFn(Inst::Or));
  }
}

TEST(InterpreterTests, RBTransferFunctions) {
  for (int WIDTH = 1; WIDTH <= MAX_WIDTH; ++WIDTH) {
    RBTesting rbObj(WIDTH);
    ASSERT_TRUE(rbObj.testFn(Inst::Add, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::AddNSW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::AddNUW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::AddNW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Sub, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::SubNSW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::SubNUW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::SubNW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Mul, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::MulNSW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::MulNUW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::MulNW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::UDiv, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::SDiv, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::UDivExact, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::SDivExact, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::URem, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::SRem, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::And, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Or, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Xor, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Shl, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::ShlNSW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::ShlNUW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::ShlNW, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::LShr, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::LShrExact, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::AShr, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::AShrExact, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Eq, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Ne, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Ult, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Slt, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Ule, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFn(Inst::Sle, CheckRBPrecision));

  }
  for (int WIDTH = 1; WIDTH <= MAX_WIDTH - 1; ++WIDTH) {
    RBTesting rbObj(WIDTH);
    ASSERT_TRUE(rbObj.testFnTernary(Inst::Select, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFnTernary(Inst::FShl, CheckRBPrecision));
    ASSERT_TRUE(rbObj.testFnTernary(Inst::FShr, CheckRBPrecision));
  }
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
  Inst *I2 = IC.createVar(8, "");
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
