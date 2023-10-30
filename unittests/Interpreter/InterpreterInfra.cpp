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

#include "InterpreterInfra.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Util/LLVMUtils.h"

namespace {
  static llvm::cl::opt<bool> DebugMode("debug-mode",
  llvm::cl::desc("Print debugging information."),
  llvm::cl::init(false));
}

using namespace souper;
using namespace llvm;

// EvalValueKB implementation
// -----------------------------

EvalValueKB::EvalValueKB(KnownBits Val) {
  K = ValueKind::Val;
  ValueKB = Val;
}

EvalValueKB::EvalValueKB(EvalValue &Val) : EvalValue(Val) {
  if (hasValue()) {
    ValueKB.One = Val.getValue();
    ValueKB.Zero = ~Val.getValue();
  }
}

KnownBits EvalValueKB::getValueKB() {
  if (K != ValueKind::Val) {
    llvm::errs() << "EvalValueKB: KnownBits not initialized.\n";
    llvm::report_fatal_error("exiting");
  }

  return ValueKB;
}

// KBTesting implementation
// -----------------------------

EvalValueKB KBTesting::merge(EvalValueKB a, EvalValueKB b) {
  if (!a.hasValue())
    return b;
  if (!b.hasValue())
    return a;

  // FIXME: Handle other ValueKind types. how?
  assert(a.hasValue() && b.hasValue());

  return EvalValueKB(KnownBitsAnalysis::mergeKnownBits({a.getValueKB(), b.getValueKB()}));
}

KnownBits KBTesting::setLowest(KnownBits x) {
  for (int i = 0; i < x.getBitWidth(); i++) {
    if (!x.Zero[i] && !x.One[i]) {
      x.One.setBit(i);
      return x;
    }
  }
  report_fatal_error("faulty setLowest!");
}

KnownBits KBTesting::clearLowest(KnownBits x) {
  for (int i = 0; i < x.getBitWidth(); i++) {
    if (!x.Zero[i] && !x.One[i]) {
      x.Zero.setBit(i);
      return x;
    }
  }
  report_fatal_error("faulty clearLowest!");
}

llvm::APInt concat(llvm::APInt A, llvm::APInt B) {
  auto W = A.getBitWidth() + B.getBitWidth();
  return (A.zext(W) << B.getBitWidth()) | B.zext(W);
}

EvalValueKB KBTesting::bruteForce(KnownBits x, KnownBits y,
                                  llvm::KnownBits z, Inst::Kind Pred) {
  if (!x.isConstant())
    return merge(bruteForce(setLowest(x), y, z, Pred),
                 bruteForce(clearLowest(x), y, z, Pred));
  if (!y.isConstant())
    return merge(bruteForce(x, setLowest(y), z,  Pred),
                 bruteForce(x, clearLowest(y), z, Pred));

  if (!z.isConstant())
    return merge(bruteForce(x, y, setLowest(z), Pred),
                 bruteForce(x, y, clearLowest(z), Pred));
  auto xc = x.getConstant();
  auto yc = y.getConstant();
  auto zc = z.getConstant();
  EvalValue Result;
  switch (Pred) {
  case Inst::Select:
    Result = (xc != 0) ? yc : zc;
    break;
  case Inst::FShl:
    Result = (concat(xc, yc) << (zc.urem(WIDTH))).trunc(WIDTH);
    break;
  case Inst::FShr:
    Result  = (concat(xc, yc).lshr(zc.urem(WIDTH))).trunc(WIDTH);
    break;
  default:
    report_fatal_error("Unhandled ternary operator.");
  }
  return Result;
}

EvalValueKB KBTesting::bruteForce(KnownBits x, KnownBits y, Inst* I) {
  if (!x.isConstant())
    return merge(bruteForce(setLowest(x), y, I),
                 bruteForce(clearLowest(x), y, I));
  if (!y.isConstant())
    return merge(bruteForce(x, setLowest(y), I),
                 bruteForce(x, clearLowest(y), I));
  auto xc = x.getConstant();
  auto yc = y.getConstant();

  ValueCache Vals{{I->Ops[0], xc}, {I->Ops[1], yc}};
  ConcreteInterpreter C(Vals);
  EvalValue Result = C.evaluateInst(I);
  return Result;
}

bool KBTesting::nextKB(llvm::KnownBits &x) {
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

bool testKB(llvm::KnownBits Calculated, EvalValueKB Expected,
            Inst::Kind K, const std::vector<llvm::KnownBits> &KBS) {
  // expected value is poison/ub; so let binary transfer functions do
  // whatever they want without complaining
  if (!Expected.hasValue())
    return true;

  if (Calculated.getBitWidth() != Expected.ValueKB.getBitWidth()) {
    llvm::errs() << "Expected and Given have unequal bitwidths - Expected: "
                 << Expected.ValueKB.getBitWidth() << ", Given: " << Calculated.getBitWidth() << '\n';
    return false;
  }
  if (Calculated.hasConflict() || Expected.ValueKB.hasConflict()) {
    llvm::errs() << "Expected or Given result has a conflict\n";
    return false;
  }

  if (KnownBitsAnalysis::isConflictingKB(Calculated, Expected.ValueKB)) {
    outs() << "Unsound!! " << Inst::getKindName(K) << "\nInputs: ";
    for (auto KB : KBS) {
      outs() << KnownBitsAnalysis::knownBitsString(KB) << " ";
    }
    outs() << "\nCalculated: " << KnownBitsAnalysis::knownBitsString(Calculated) << '\n';
    outs() << "Expected: " << KnownBitsAnalysis::knownBitsString(Expected.ValueKB) << '\n';
    return false;
  }
  return true;
}

bool KBTesting::testTernaryFn(Inst::Kind K, size_t Op0W,
                              size_t Op1W, size_t Op2W) {
  llvm::KnownBits x(Op0W);
  do {
    llvm::KnownBits y(Op1W);
    do {
      llvm::KnownBits z(Op2W);
      do {
        InstContext IC;
        auto Op0 = IC.createVar(Op0W, "Op0");
        auto Op1 = IC.createVar(Op1W, "Op1");
        auto Op2 = IC.createVar(Op2W, "Op2");
        auto I = IC.getInst(K, WIDTH, {Op0, Op1, Op2});
        std::unordered_map<Inst *, llvm::KnownBits> C{{Op0, x}, {Op1, y}, {Op2, z}};
        KnownBitsAnalysis KB(C);
        ConcreteInterpreter BlankCI;
        auto Calculated = KB.findKnownBits(I, BlankCI, false);
        auto Expected = bruteForce(x, y, z, K);
        if (!testKB(Calculated, Expected, K, {x, y, z})) {
          return false;
        }
      } while (nextKB(z));
    } while (nextKB(y));
  } while (nextKB(x));

  return true;
}

bool KBTesting::testFn(Inst::Kind K) {
  llvm::KnownBits x(WIDTH);
  do {
    llvm::KnownBits y(WIDTH);
    do {
      InstContext IC;
      auto Op0 = IC.createVar(WIDTH, "Op0");
      auto Op1 = IC.createVar(WIDTH, "Op1");
      auto I = IC.getInst(K, WIDTH, {Op0, Op1});
      std::unordered_map<Inst *, llvm::KnownBits> C{{Op0, x}, {Op1, y}};
      KnownBitsAnalysis KB(C);
      ConcreteInterpreter BlankCI;
      auto Calculated = KB.findKnownBits(I, BlankCI, false);
      EvalValueKB Expected = bruteForce(x, y, I);
      if (!testKB(Calculated, Expected, K, {x, y}))
        return false;
    } while(nextKB(y));
  } while(nextKB(x));

  return true;
}


// CRTesting Implementation
// -----------------------------

bool CRTesting::rangeContainsAll(const ConstantRange &R, const bool Table[]) {
  const int Range = 1 << WIDTH;
  for (int i = 0; i < Range; ++i) {
    if (Table[i]) {
      APInt a(WIDTH, i);
      if (!R.contains(a))
        return false;
    }
  }
  return true;
}

// Find the largest hole and build a ConstantRange around it
ConstantRange CRTesting::bestCR(const bool Table[], const int Width) {
  const int Range = 1 << Width;
  unsigned Pop = 0;
  unsigned Any;
  for (unsigned i = 0; i < Range; ++i)
    if (Table[i]) {
      ++Pop;
      Any = i;
    }
  if (Pop == 0)
    return ConstantRange(Width, /*isFullSet=*/false);
  if (Pop == Range)
    return ConstantRange(Width, /*isFullSet=*/true);

  unsigned Hole = 0, MaxHole = 0, MaxSize = 0;
  bool inHole = false;
  for (unsigned i = 0; i < Range; ++i) {
    if (Table[i]) {
      if (inHole) {
        inHole = false;
        if ((i - Hole) > MaxSize) {
          MaxHole = Hole;
          MaxSize = i - Hole;
        }
      }
    } else {
      if (!inHole) {
        inHole = true;
        Hole = i;
      }
    }
  }
  if (inHole && ((Range - Hole) > MaxSize)) {
    MaxHole = Hole;
    MaxSize = Range - Hole;
  }

  unsigned Bottom = 0;
  while (!Table[Bottom])
    ++Bottom;
  unsigned Top = Range - 1;
  while (!Table[Top])
    --Top;

  ConstantRange R(Width, false);
  if ((Bottom + (Range - 1 - Top)) > MaxSize) {
    APInt Lo(Width, Bottom);
    APInt Hi(Width, (Top + 1) % Range);
    R = ConstantRange(Lo, Hi);
  } else {
    APInt Lo(Width, (MaxHole + MaxSize) % Range);
    APInt Hi(Width, MaxHole);
    R = ConstantRange(Lo, Hi);
  }

  assert(rangeContainsAll(R, Table));
  if (Pop == 1) {
    assert(R.getLower().getLimitedValue() == Any);
    assert(R.getUpper().getLimitedValue() == (Any + 1) % Range);
  } else {
    APInt L1 = R.getLower() + 1;
    ConstantRange R2(L1, R.getUpper());
    assert(!rangeContainsAll(R2, Table));
    ConstantRange R3(R.getLower(), R.getUpper() - 1);
    assert(!rangeContainsAll(R3, Table));
  }

  return R;
}

ConstantRange CRTesting::exhaustive(const ConstantRange &L, const ConstantRange &R,
                                    Inst::Kind pred, const ConstantRange &Untrusted) {
  if (L.isEmptySet() || R.isEmptySet())
    return ConstantRange(WIDTH, /*isFullSet=*/false);
  bool Table[1 << WIDTH];
  for (int i = 0; i < (1 << WIDTH); ++i)
    Table[i] = false;
  auto LI = L.getLower();
  do {
    auto RI = R.getLower();
    do {
      APInt Val;
      switch (pred) {
      case Inst::And:
        Val = LI & RI;
        break;
      case Inst::Or:
        Val = LI | RI;
        break;
      case Inst::Add:
        Val = LI + RI;
        break;
      case Inst::Sub:
        Val = LI - RI;
        break;
      case Inst::Shl:
        Val = LI.shl(RI);
        break;
      case Inst::LShr:
        Val = LI.lshr(RI);
        break;
      case Inst::AShr:
        Val = LI.ashr(RI);
        break;
      default:
        report_fatal_error("unknown opcode");
      }
      if (!Untrusted.contains(Val)) {
        outs() << "Unsound! " << Inst::getKindName(pred) << '\n';
        outs() << L << ' ' << Inst::getKindName(pred) << ' '
               << R << '\n';
        outs() << "Calculated value " << Untrusted <<  " must contain: " << Val << '\n';
        report_fatal_error("Unsound!");
      }
      Table[Val.getLimitedValue()] = true;
      ++RI;
    } while (RI != R.getUpper());
    ++LI;
  } while (LI != L.getUpper());
  return bestCR(Table, WIDTH);
}

void CRTesting::check(const ConstantRange &L, const ConstantRange &R, Inst::Kind pred) {
  ConstantRange FastRes(WIDTH, true);
  switch (pred) {
  case Inst::Or:
    FastRes = BinaryTransferFunctionsCR::binaryOr(L, R);
    break;
  case Inst::And:
    FastRes = BinaryTransferFunctionsCR::binaryAnd(L, R);
    break;
  default:
    report_fatal_error("unsupported opcode");
  }
  ConstantRange PreciseRes = exhaustive(L, R, pred, FastRes);
  assert(getSetSize(PreciseRes).ule(getSetSize(FastRes)));
}

ConstantRange CRTesting::nextCR(const ConstantRange &CR) {
  auto L = CR.getLower();
  auto U = CR.getUpper();
  do {
    if (U.isMaxValue())
      ++L;
    ++U;
  } while (L == U && !L.isMinValue() && !L.isMaxValue());
  return ConstantRange(L, U);
}

bool CRTesting::testFn(Inst::Kind pred) {
  ConstantRange L(WIDTH, /*isFullSet=*/false);
  ConstantRange R(WIDTH, /*isFullSet=*/false);
  do {
    do {
      check(L, R, pred);
      R = nextCR(R);
    } while (!R.isEmptySet());
    L = nextCR(L);
  } while (!L.isEmptySet());

  return true;
}


// RBTesting Implementation
// -----------------------------

bool RBTesting::nextRB(llvm::APInt &Val) {
  if (Val.isAllOnes()) {
    return false;
  } else {
    ++Val;
    return true;
  }
}

std::vector<llvm::APInt> explodeUnrestrictedBits(const llvm::APInt &Value,
                                                 const llvm::APInt &Mask,
                                                 const int WIDTH) {
  std::vector<llvm::APInt> Result { Value };

  for (int i = 0; i < WIDTH; ++i) {
    if ( (Mask & (1 << i)) == 0 ) {
      auto Copy = Result;
      for (auto &Y : Copy) {
        Result.push_back(Y | (1 << i));
      }
    }
  }
  return Result;
}

// Probably refactor to unify these two
std::vector<llvm::APInt> explodeRestrictedBits(const llvm::APInt &X,
                                               const int WIDTH) {
  std::vector<llvm::APInt> Result { llvm::APInt(WIDTH, 0) };

  for (int i = 0; i < WIDTH; ++i) {
    if ( (X & (1 << i)) != 0 ) {
      auto Copy = Result;
      for (auto &Y : Copy) {
        Result.push_back(Y | 1 << i);
      }
    }
  }
  return Result;
}

llvm::APInt exhaustiveRB(Inst *I, Inst *X, Inst *Y,
                         const llvm::APInt &RBX, const llvm::APInt &RBY,
                         const int WIDTH) {
  llvm::APInt RB(WIDTH, 0);
  auto XPartial = explodeRestrictedBits(RBX, X->Width);
  auto YPartial = explodeRestrictedBits(RBY, Y->Width);
  int cases = 0;
  for (auto P0 : XPartial) {
    for (auto P1 : YPartial) {

      if (DebugMode) {
        llvm::outs() << "Restricted EXP: " << getPaddedBinaryString(P0) << "\t"
                    << getPaddedBinaryString(P1) << "\n";
      }

      auto I0 = explodeUnrestrictedBits(P0, P0 | RBX, X->Width);
      auto I1 = explodeUnrestrictedBits(P1, P1 | RBY, Y->Width);

      std::map<int, std::pair<bool, bool>> Seen;

      for (auto i0 : I0) {
        for (auto i1 : I1) {
          if (DebugMode) {
            llvm::outs() << "Unrestricted EXP: " << getPaddedBinaryString(i0) << "\t"
                         << getPaddedBinaryString(i1) << "\n";
          }
          ++cases;
          ValueCache C = {{{X, i0}, {Y, i1}}};
          ConcreteInterpreter Interp(C);
          auto Val_ = Interp.evaluateInst(I);
          if (!Val_.hasValue()) {
            continue;
          }
          auto Val = Val_.getValue();

          for (int i = 0; i < WIDTH; ++i) {
            if ((Val & (1 << i)) == 0) Seen[i].first = true;
            if ((Val & (1 << i)) != 0) Seen[i].second = true;
          }

        }
      }
      for (int i = 0 ; i < WIDTH; ++i) {
        if (!Seen[i].first || !Seen[i].second) {
          RB = RB | (1 << i);
        }
      }
    }
  }
  if (DebugMode) {
    llvm::outs() << "Cases : " << cases << "\n";
  }
  return RB;
}

void compareRB(const llvm::APInt &RBComputed,
               const llvm::APInt &RBExhaustive,
               bool &fail, bool &FoundMorePrecise,
               double &ImpreciseCount) {
  for (int i = 0; i < RBComputed.getBitWidth(); ++i) {
    if (((RBComputed & (1 << i)) == 0) && ((RBExhaustive & (1 << i)) != 0))
      fail = true;
    if (((RBExhaustive & (1 << i)) == 0) && ((RBComputed & (1 << i)) != 0)) {
      FoundMorePrecise = true;
      // FIXME add the number of bits, not the number of incorrect cases
      ++ImpreciseCount;
    }
  }
}

bool RBTesting::testFn(const Inst::Kind K, const bool CheckPrecision) {
  llvm::APInt RB0(WIDTH, 0);
  InstContext IC;
  Inst *X = IC.createVar(WIDTH, "X");
  Inst *Y = IC.createVar(WIDTH, "Y");
  std::pair<size_t, size_t> Stats;
  double ImpreciseCount = 0;

  do {
    llvm::APInt RB1(WIDTH, 0);
    do {
      auto EffectiveWidth = WIDTH;
      if (K == Inst::Eq || K == Inst::Ne || K == Inst::Sle
          || K == Inst::Slt || K == Inst::Ule || K == Inst::Ult) {
        EffectiveWidth = 1;
      }
      auto Expr = IC.getInst(K, EffectiveWidth, {X, Y});
      RestrictedBitsAnalysis RBA{{{X, RB0}, {Y, RB1}}};

      if (DebugMode) {
        llvm::outs() << "RBInputs : " << getPaddedBinaryString(RB0) << "\t"
                     << getPaddedBinaryString(RB1) << "\n";
      }
      auto RBComputed = RBA.findRestrictedBits(Expr);
      auto RBExhaustive = exhaustiveRB(Expr, X, Y, RB0, RB1, EffectiveWidth);
      bool fail = false;
      bool FoundMorePrecise = false;
      compareRB(RBComputed, RBExhaustive, fail, FoundMorePrecise, ImpreciseCount);
      Stats.first++;
      if (fail) {
        llvm::outs() << Inst::getKindName(K) << ":\t";
        llvm::outs() << "Inputs: " << getPaddedBinaryString(RB0) << ", "
                     << getPaddedBinaryString(RB1) << "\n";
        llvm::outs() << "Computed:   " << getPaddedBinaryString(RBComputed) << "\n";
        llvm::outs() << "Exhaustive: " << getPaddedBinaryString(RBExhaustive) << " <-- UNSOUND!!!!\n";
        return false;
      }
      if (CheckPrecision) {
        llvm::outs() << Inst::getKindName(K) << ":\t";
        llvm::outs() << "Inputs: " << getPaddedBinaryString(RB0) << ", "
                     << getPaddedBinaryString(RB1) << "\t";
        llvm::outs() << "Computed:\t" << getPaddedBinaryString(RBComputed) << "\t";
        llvm::outs() << "Exhaustive:\t" << getPaddedBinaryString(RBExhaustive);
        if (FoundMorePrecise) {
          llvm::outs() << "  <-- imprecise!";
          Stats.second++;
        }
        llvm::outs() << "\n";
      }
    } while (nextRB(RB1));
  } while (nextRB(RB0));
  if (CheckPrecision) {
      llvm::outs() << "TOTAL imprecise results: " <<  Inst::getKindName(K) << " : "
                   << Stats.second << "/" << Stats.first << "\n";
      llvm::outs() << "TOTAL imprecise bits: " << ImpreciseCount << "\n\n";
  }
  return true;
}

llvm::APInt exhaustiveRBTernary(Inst *I, Inst *X, Inst *Y, Inst *Z,
                                const llvm::APInt &RBX, const llvm::APInt &RBY,
                                const llvm::APInt &RBZ,
                                const int WIDTH) {
  llvm::APInt RB(WIDTH, 0);
  auto XPartial = explodeRestrictedBits(RBX, X->Width);
  auto YPartial = explodeRestrictedBits(RBY, Y->Width);
  auto ZPartial = explodeRestrictedBits(RBZ, Z->Width);
  int cases = 0;
  double ImpreciseCount = 0;

  for (auto P0 : XPartial) {
    for (auto P1 : YPartial) {
      for (auto P2 : ZPartial) {
        if (DebugMode) {
          llvm::outs() << "Restricted EXP: " << getPaddedBinaryString(P0) << "\t"
                       << getPaddedBinaryString(P1) << "\t"
                       << getPaddedBinaryString(P2) << "\n";
        }

        auto I0 = explodeUnrestrictedBits(P0, P0 | RBX, X->Width);
        auto I1 = explodeUnrestrictedBits(P1, P1 | RBY, Y->Width);
        auto I2 = explodeUnrestrictedBits(P2, P2 | RBZ, Z->Width);
        std::map<int, std::pair<bool, bool>> Seen;

        for (auto i0 : I0) {
          for (auto i1 : I1) {
            for (auto i2 : I2) {
              if (DebugMode) {
                llvm::outs() << "Unrestricted EXP: " << getPaddedBinaryString(i0) << "\t"
                             << getPaddedBinaryString(i1) << "\t"
                             << getPaddedBinaryString(i2) << "\n";
              }
              ++cases;
              ValueCache C = {{{X, i0}, {Y, i1}, {Z, i2}}};
              ConcreteInterpreter Interp(C);
              auto Val_ = Interp.evaluateInst(I);
              if (!Val_.hasValue()) {
                continue;
              }
              auto Val = Val_.getValue();
              for (int i = 0; i < WIDTH; ++i) {
                if ((Val & (1 << i)) == 0) Seen[i].first = true;
                if ((Val & (1 << i)) != 0) Seen[i].second = true;
              }
            }
          }
        }
        for (int i = 0; i < WIDTH; ++i) {
          if (!Seen[i].first || !Seen[i].second) {
            RB = RB | (1 << i);
          }
        }
      }
    }
  }
  if (DebugMode) {
    llvm::outs() << "Cases : " << cases << "\n";
  }
  return RB;
}

bool RBTesting::testFnTernary(const Inst::Kind K, const bool CheckPrecision) {
  llvm::APInt RB0(WIDTH, 0);
  InstContext IC;
  Inst *X = IC.createVar(WIDTH, "X");
  if (K == Inst::Select) {
    X = IC.createVar(1, "X");
    RB0 = llvm::APInt(1, 0);
  }
  Inst *Y = IC.createVar(WIDTH, "Y");
  Inst *Z = IC.createVar(WIDTH, "Z");
  std::pair<size_t, size_t> Stats;
  double ImpreciseCount = 0;

  do {
    llvm::APInt RB1(WIDTH, 0);
    do {
      llvm::APInt RB2(WIDTH, 0);
      do {
        auto EffectiveWidth = WIDTH;
        if (K == Inst::Eq || K == Inst::Ne || K == Inst::Sle
            || K == Inst::Slt || K == Inst::Ule || K == Inst::Ult) {
          EffectiveWidth = 1;
        }
        std::vector<Inst *> Ops = {X, Y, Z};
        std::unordered_map<Inst *, llvm::APInt> RBCache = {{X, RB0},
                                                           {Y, RB1},
                                                           {Z, RB2}};
        auto Expr = IC.getInst(K, EffectiveWidth, Ops);
        RestrictedBitsAnalysis RBA{RBCache};

        if (DebugMode) {
          llvm::outs() << "RBInputs : " << getPaddedBinaryString(RB0) << "\t"
                       << getPaddedBinaryString(RB1) << '\t'
                       << getPaddedBinaryString(RB2);
          llvm::outs() << '\n';
        }

        auto RBComputed = RBA.findRestrictedBits(Expr);
        auto RBExhaustive = exhaustiveRBTernary(Expr, X, Y, Z, RB0, RB1, RB2, EffectiveWidth);
        bool fail = false;
        bool FoundMorePrecise = false;
        compareRB(RBComputed, RBExhaustive, fail, FoundMorePrecise, ImpreciseCount);
        Stats.first++;
        if (CheckPrecision || fail) {
          llvm::outs() << Inst::getKindName(K) << ":\t";
          llvm::outs() << "Inputs: " << getPaddedBinaryString(RB0) << ", "
                       << getPaddedBinaryString(RB1) << ", "
                       << getPaddedBinaryString(RB2);
          llvm::outs() << "\t";
          llvm::outs() << "Computed:   " << getPaddedBinaryString(RBComputed) << "\t";
          llvm::outs() << "Exhaustive: " << getPaddedBinaryString(RBExhaustive);
          if (fail) {
            llvm::outs() << " <-- UNSOUND!!!!\n";
            return false;
          } else if (FoundMorePrecise) {
            llvm::outs() << "  <-- imprecise!";
            Stats.second++;
          }
          llvm::outs() << "\n";
        }
      } while (nextRB(RB2));
    } while (nextRB(RB1));
  } while (nextRB(RB0));
  if (CheckPrecision) {
    llvm::outs() << "TOTAL imprecise results: " << Inst::getKindName(K) << " : "
                 << Stats.second << "/" << Stats.first << "\n";
    llvm::outs() << "TOTAL imprecise bits: " << ImpreciseCount << "\n\n";
  }
  return true;
}

