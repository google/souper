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

#include "llvm/Support/raw_ostream.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"

using namespace souper;
using namespace llvm;

// CR KB reduction
void TestingUtil::exhaustiveKBCRReduction(KnownBits &KB, ConstantRange &CR) {
  constexpr unsigned SetSize = 1 << WIDTH;
  if (CR.isEmptySet()) {
    KB.One = 0;
    KB.Zero = ~0;

    return;
  }
  // concretize both
  std::bitset<SetSize> KBSet = concretizeKB<SetSize>(KB);
  std::bitset<SetSize> CRSet = concretizeCR<SetSize>(CR);

  // intersect
  std::bitset<SetSize> IntersectResult = KBSet & CRSet;

  // abstractize both to KB and CR
  KnownBits FinalKB = abstractizeKB<SetSize>(KBSet);
  ConstantRange FinalCR = abstractizeCR<SetSize>(CRSet);

  KB = FinalKB;
  CR = FinalCR;
}


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

EvalValueKB KBTesting::bruteForce(KnownBits x, KnownBits y, Inst::Kind Pred) {
  if (!x.isConstant())
    return merge(bruteForce(setLowest(x), y, Pred),
		 bruteForce(clearLowest(x), y, Pred));
  if (!y.isConstant())
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

bool KBTesting::testFn(Inst::Kind pred) {
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

      if (KnownBitsAnalysis::isConflictingKB(Calculated, Expected.ValueKB)) {
	outs() << "Unsound!! " << Inst::getKindName(pred) << '\n';
	outs() << KnownBitsAnalysis::knownBitsString(x) << ' ' << Inst::getKindName(pred)
	       << ' ' << KnownBitsAnalysis::knownBitsString(y) << '\n';
	outs() << "Calculated: " << KnownBitsAnalysis::knownBitsString(Calculated) << '\n';
	outs() << "Expected: " << KnownBitsAnalysis::knownBitsString(Expected.ValueKB) << '\n';
	return false;
      }

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

void CRTesting::check(const ConstantRange &L, const ConstantRange &R, Inst::Kind pred,
		      double &FastBits, double &PreciseBits, int &Count, int &PreciseCount) {
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

  long FastSize = FastRes.getSetSize().getLimitedValue();
  long PreciseSize = PreciseRes.getSetSize().getLimitedValue();

  assert(FastSize >= 0 && FastSize <= (1 << WIDTH));
  assert(PreciseSize >= 0 && PreciseSize <= (1 << WIDTH));
  assert(PreciseSize <= FastSize);

  if (FastSize > 0) {
    FastBits += log2((double)FastSize);
    Count++;
  }
  if (PreciseSize > 0) {
    PreciseBits += log2((double)PreciseSize);
    PreciseCount++;
  }
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
  double FastBits = 0.0, PreciseBits = 0.0;
  long count = 0;
  int Count = 0, PreciseCount = 0;
  do {
    do {
      check(L, R, pred, FastBits, PreciseBits, Count, PreciseCount);
      R = nextCR(R);
    } while (!R.isEmptySet());
    L = nextCR(L);
  } while (!L.isEmptySet());

  return true;
}
