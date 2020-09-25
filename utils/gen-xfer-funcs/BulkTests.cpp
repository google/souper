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

#include "llvm/IR/ConstantRange.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/Support/raw_ostream.h"

#include "InterpreterInfra.h"
#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Inst/Inst.h"
#include "gtest/gtest.h"

#include "funcs.h"
#include <dlfcn.h>
#include <iostream>

using namespace llvm;
using namespace souper;

unsigned DebugLevel;

namespace {

//const auto K = Inst::And;
//const auto K = Inst::Or;
//const auto K = Inst::Xor;
const auto K = Inst::Add;
//const auto K = Inst::Sub;
//const auto K = Inst::Mul;
//const auto K = Inst::URem;
//const auto K = Inst::Shl;
//const auto K = Inst::MulNW;
//const auto K = Inst::UDiv;
//const auto K = Inst::LShr;
//const auto K = Inst::SDiv;

// TODO expose to command line
const bool ARG0CONST = false;
const bool ARG1CONST = false;

// TODO expose to command line

#if 1

#define MAX_W 6

#else

#define TESTONE 1
#define MAX_W 6
//#define VERBOSE 0

#endif

#ifdef TESTONE
static KnownBits f(const KnownBits &x, const KnownBits &y) {
  const int WIDTH = x.One.getBitWidth();
  const APInt t0 = operator+(y.One, x.One);
  const APInt t1 = ~y.Zero;
  const APInt t2 = -t1;
  const APInt t3 = -x.Zero;
  const APInt t4 = -t3;
  const APInt t5 = ~t4;
  const APInt t6 = operator-(t5, t2);
  const APInt t7 = (APInt::getAllOnesValue(WIDTH).getBoolValue()) ? (t6) : (x.Zero);
  const APInt t8 = operator&(t7, t0);
  const APInt t9 = operator|(x.Zero, y.Zero);
  const APInt t10 = operator&(t8, t9);
  KnownBits ret(WIDTH);
  ret.One = t10;
  ret.Zero = APInt::getMinValue(WIDTH);
  return ret;
}
#endif

struct triple {
  KnownBits In0, In1, Out;
};

std::vector<std::vector<triple>> Oracle;

bool nextTrivialKB(llvm::KnownBits &x) {
  x.One += 1;
  x.Zero = ~x.One;
  return x.One != 0;
}

void genKB(Inst::Kind K, int W) {
  int val = 0, noVal = 0;
  llvm::KnownBits x(W);
  if (ARG0CONST)
    x.Zero.setAllBits();
  do {
    llvm::KnownBits y(W);
    if (ARG1CONST)
      y.Zero.setAllBits();
    do {
      KBTesting kbObj(W);
      InstContext IC;
      auto Op0 = IC.createVar(W, "Op0");
      auto Op1 = IC.createVar(W, "Op1");
      auto I = IC.getInst(K, W, {Op0, Op1});
      std::unordered_map<Inst *, llvm::KnownBits> C{{Op0, x}, {Op1, y}};
      KBTesting KBTest(W);
      EvalValueKB Res = KBTest.bruteForce(x, y, I);
      if (!Res.hasValue()) {
        ++noVal;
        continue;
      }
      KnownBits KB = Res.getValueKB();
      if (KB.hasConflict())
        llvm::report_fatal_error("conflict in oracle");
      ++val;
      Oracle[W].push_back({x, y, KB});
    } while (ARG1CONST ? nextTrivialKB(y) : KBTesting::nextKB(y));
  } while (ARG0CONST ? nextTrivialKB(x) : KBTesting::nextKB(x));
  llvm::outs() << "at width = " << W << ", " << val << " have values, " << noVal
               << " do not\n";
}

void compare(const KnownBits &Calculated, const KnownBits &Expected,
             long &ImpreciseBits, long &ImpreciseZeroes, long &ImpreciseOnes,
             long &UnsoundBits, long &MaxKnown, long &ActualKnown) {
  for (int i = 0; i < Expected.getBitWidth(); ++i) {
    if (Calculated.One[i] || Calculated.Zero[i])
      ++ActualKnown;
    if (Expected.Zero[i]) {
      ++MaxKnown;
      if (Calculated.One[i])
        ++UnsoundBits;
      if (!Calculated.Zero[i]) {
        ++ImpreciseBits;
        ++ImpreciseZeroes;
      }
    }
    if (Expected.One[i]) {
      ++MaxKnown;
      if (Calculated.Zero[i])
        ++UnsoundBits;
      if (!Calculated.One[i]) {
        ++ImpreciseBits;
        ++ImpreciseOnes;
      }
    }
    if (!Expected.Zero[i] && !Expected.One[i]) {
      if (Calculated.Zero[i] || Calculated.One[i])
        ++UnsoundBits;
    }
    if (Calculated.Zero[i] && Calculated.One[i])
      ++UnsoundBits;
  }
}

void checkKB(Inst::Kind K, TestFn Func, int W,
             long &ImpreciseBits,
             long &ImpreciseZeroes,
             long &ImpreciseOnes,
             long &UnsoundBits,
             long &MaxKnown,
             long &ActualKnown) {
  for (auto &t : Oracle[W]) {
    KnownBits Cand = Func(t.In0, t.In1);
    if (Cand.hasConflict()) {
      UnsoundBits += 10000;
      return;
    }
#if defined(VERBOSE) && defined(TESTONE)
    llvm::outs() << KnownBitsAnalysis::knownBitsString(t.In0) << " op "
                 << KnownBitsAnalysis::knownBitsString(t.In1) << " = "
                 << KnownBitsAnalysis::knownBitsString(Cand) << " (expecting "
                 << KnownBitsAnalysis::knownBitsString(t.Out) << ") ";
    int oldImprecise = ImpreciseBits;
    int oldUnsound = UnsoundBits;
#endif
    compare(Cand, t.Out, ImpreciseBits, ImpreciseZeroes, ImpreciseOnes,
            UnsoundBits, MaxKnown, ActualKnown);
#if defined(VERBOSE) && defined(TESTONE)
    llvm::outs() << (ImpreciseBits - oldImprecise) << " "
                 << ((UnsoundBits - oldUnsound) ? "<--- unsound!!!" : "")
                 << "\n";
#endif
  }
}

} // namespace

int main(int argc, char *argv[]) {
  if (argc != 2) {
    llvm::errs() << "usage: bulk_tests /full/path/to/file.so\n";
    exit(1);
  }

  for (int W = 0; W <= MAX_W; ++W) {
    Oracle.push_back(std::vector<triple>());
    if (W >= 1)
      genKB(K, W);
  }

#ifdef TESTONE
  for (int W = 1; W <= MAX_W; ++W) {
    long ImpreciseBits = 0, UnsoundBits = 0, MaxKnown = 0,
      ActualKnown = 0, ImpreciseZeroes = 0, ImpreciseOnes = 0;
    checkKB(K, f, W, ImpreciseBits, ImpreciseZeroes, ImpreciseOnes,
            UnsoundBits, MaxKnown, ActualKnown);
    llvm::outs() << W << "\n";
    llvm::outs() << "  actual known bits: " << ActualKnown << "\n";
    llvm::outs() << "  max known bits: " << MaxKnown << "\n";
    llvm::outs() << "  precision = " << std::to_string(100.0 * ActualKnown / MaxKnown) << "%\n";
    llvm::outs() << "  unsound: " << UnsoundBits << "\n";
    llvm::outs() << "  imprecise: " << ImpreciseBits << "\n";
    llvm::outs() << "  imprecise zeroes: " << ImpreciseZeroes << "\n";
    llvm::outs() << "  imprecise ones  : " << ImpreciseOnes << "\n";
  }
#else
  char *FuncFile = argv[1];

  void *handle = dlopen(FuncFile, RTLD_LAZY);
  if (!handle) {
    llvm::errs() << dlerror() << "\n";
    exit(1);
  }

  TestFn *Funcs = (TestFn *)dlsym(handle, "AllFuncs");
  if (char *error = dlerror()) {
    llvm::errs() << error << "\n";
    exit(1);
  }

  for (int I = 0; Funcs[I]; ++I) {
    llvm::outs() << I << " ";

    llvm::outs() << (void *)Funcs[I] << " ";

    long ImpreciseBits = 0, UnsoundBits = 0, MaxKnown = 0, ActualKnown = 0;
    long ImpreciseZeroes = 0, ImpreciseOnes = 0;
    for (int W = 1; W <= MAX_W; ++W)
      checkKB(K, Funcs[I], W, ImpreciseBits, ImpreciseZeroes, ImpreciseOnes,
              UnsoundBits, MaxKnown, ActualKnown);
    double Score = (UnsoundBits * 1000) + ImpreciseBits;
    llvm::outs() << "score = " << (long)Score << "\n";
  }

  dlclose(handle);
#endif
}
