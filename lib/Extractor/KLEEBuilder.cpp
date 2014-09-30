// Copyright 2014 The Souper Authors. All rights reserved.
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

#include "souper/Extractor/KLEEBuilder.h"

#include "klee/Expr.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/util/ExprSMTLIBLetPrinter.h"
#include "klee/util/Ref.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <functional>
#include <map>
#include <memory>
#include <sstream>

static llvm::cl::opt<bool> DumpKLEEExprs(
    "dump-klee-exprs",
    llvm::cl::desc("Dump KLEE expressions after SMTLIB queries"),
    llvm::cl::init(false));

using namespace llvm;
using namespace klee;
using namespace souper;

namespace {

struct PhiPath {
  std::map<Block *, unsigned> BlockConstraints;
  std::vector<Inst *> Phis;
  std::vector<Inst *> UBInsts;
};

struct ExprBuilder {
  ExprBuilder(std::vector<std::unique_ptr<Array> > &Arrays,
              std::vector<Inst *> &ArrayVars)
      : Arrays(Arrays), ArrayVars(ArrayVars) {}

  std::map<Block *, std::vector<ref<Expr> >> BlockPredMap;
  std::map<Inst *, ref<Expr> > ExprMap;
  std::map<Inst *, ref<Expr> > UBExprMap;
  std::vector<std::unique_ptr<Array>> &Arrays;
  std::vector<Inst *> &ArrayVars;
  std::vector<Inst *> PhiInsts;
  UniqueNameSet ArrayNames;

  ref<Expr> makeSizedArrayRead(unsigned Width, StringRef Name, Inst *Origin);
  ref<Expr> addnswUB(Inst *I);
  ref<Expr> addnuwUB(Inst *I);
  ref<Expr> subnswUB(Inst *I);
  ref<Expr> subnuwUB(Inst *I);
  ref<Expr> mulnswUB(Inst *I);
  ref<Expr> mulnuwUB(Inst *I);
  ref<Expr> udivUB(Inst *I);
  ref<Expr> udivExactUB(Inst *I);
  ref<Expr> sdivUB(Inst *I);
  ref<Expr> sdivExactUB(Inst *I);
  ref<Expr> shiftUB(Inst *I);
  ref<Expr> shlnswUB(Inst *I);
  ref<Expr> shlnuwUB(Inst *I);
  ref<Expr> lshrExactUB(Inst *I);
  ref<Expr> ashrExactUB(Inst *I);
  ref<Expr> countOnes(ref<Expr> E);
  ref<Expr> buildAssoc(std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
                       llvm::ArrayRef<Inst *> Ops);
  ref<Expr> build(Inst *I);
  ref<Expr> get(Inst *I);
  ref<Expr> getInstMapping(const InstMapping &IM);
  std::vector<ref<Expr >> getBlockPredicates(Inst *I);
  ref<Expr> getUBInstCondition();
  void getUBPhiPaths(Inst *I, PhiPath *Current,
                     std::vector<std::unique_ptr<PhiPath>> &Paths);
};

}

ref<Expr> ExprBuilder::makeSizedArrayRead(unsigned Width, StringRef Name,
                                          Inst *Origin) {
  std::string NameStr;
  if (Name.empty())
    NameStr = "arr";
  else if (Name[0] >= '0' && Name[0] <= '9')
    NameStr = ("a" + Name).str();
  else
    NameStr = Name;
  Arrays.emplace_back(
      new Array(ArrayNames.makeName(NameStr), 1, 0, 0, Expr::Int32, Width));
  ArrayVars.push_back(Origin);

  UpdateList UL(Arrays.back().get(), 0);
  return ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
}

ref<Expr> ExprBuilder::addnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Add = AddExpr::create(L, R);
   Expr::Width Width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, Width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, Width-1, Expr::Bool);
   ref<Expr> AddMSB = ExtractExpr::create(Add, Width-1, Expr::Bool);
   return Expr::createImplies(EqExpr::create(LMSB, RMSB),
                              EqExpr::create(LMSB, AddMSB));
}

ref<Expr> ExprBuilder::addnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width Width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, Width+1);
   ref<Expr> Rext = ZExtExpr::create(R, Width+1);
   ref<Expr> Add = AddExpr::create(Lext, Rext);
   ref<Expr> AddMSB = ExtractExpr::create(Add, Width, Expr::Bool);
   return Expr::createIsZero(AddMSB);
}

ref<Expr> ExprBuilder::subnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Sub = SubExpr::create(L, R);
   Expr::Width Width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, Width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, Width-1, Expr::Bool);
   ref<Expr> SubMSB = ExtractExpr::create(Sub, Width-1, Expr::Bool);
   return Expr::createImplies(NeExpr::create(LMSB, RMSB),
                              EqExpr::create(LMSB, SubMSB));
}

ref<Expr> ExprBuilder::subnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width Width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, Width+1);
   ref<Expr> Rext = ZExtExpr::create(R, Width+1);
   ref<Expr> Sub = SubExpr::create(Lext, Rext);
   ref<Expr> SubMSB = ExtractExpr::create(Sub, Width, Expr::Bool);
   return Expr::createIsZero(SubMSB);
}

ref<Expr> ExprBuilder::mulnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = SExtExpr::create(get(Ops[0]), 2*I->Width);
   ref<Expr> R = SExtExpr::create(get(Ops[1]), 2*I->Width);
   ref<Expr> Mul = MulExpr::create(L, R);
   ref<Expr> LowerBits = ExtractExpr::create(Mul, 0, I->Width);
   ref<Expr> LowerBitsExt = SExtExpr::create(LowerBits, 2*I->Width);
   return EqExpr::create(Mul, LowerBitsExt);
}

ref<Expr> ExprBuilder::mulnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width Width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, 2*Width);
   ref<Expr> Rext = ZExtExpr::create(R, 2*Width);
   ref<Expr> Mul = MulExpr::create(Lext, Rext);
   ref<Expr> HigherBits = ExtractExpr::create(Mul, Width, Width);
   return Expr::createIsZero(HigherBits);
}

ref<Expr> ExprBuilder::udivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> R = get(Ops[1]);
   return NeExpr::create(R, klee::ConstantExpr::create(0, R->getWidth()));
}

ref<Expr> ExprBuilder::udivExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Udiv = UDivExpr::create(L, R);
   return EqExpr::create(L, MulExpr::create(R, Udiv));
}

ref<Expr> ExprBuilder::sdivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> ShiftBy = klee::ConstantExpr::create(L->getWidth()-1, L->getWidth());
   ref<Expr> IntMin = ShlExpr::create(klee::ConstantExpr::create(
                                      1, L->getWidth()), ShiftBy);
   ref<Expr> NegOne = AShrExpr::create(IntMin, ShiftBy);
   return AndExpr::create(NeExpr::create(R, klee::ConstantExpr::create(
                          0, R->getWidth())), OrExpr::create(
                          NeExpr::create(L, IntMin), NeExpr::create(R, NegOne)));
}

ref<Expr> ExprBuilder::sdivExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Sdiv = SDivExpr::create(L, R);
   return EqExpr::create(L, MulExpr::create(R, Sdiv));
}

ref<Expr> ExprBuilder::shiftUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Lwidth = klee::ConstantExpr::create(L->getWidth(), L->getWidth());
   return UltExpr::create(R, Lwidth);
}

ref<Expr> ExprBuilder::shlnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = ShlExpr::create(L, R);
   ref<Expr> RShift = AShrExpr::create(Result, R);
   return EqExpr::create(RShift, L);
}

ref<Expr> ExprBuilder::shlnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = ShlExpr::create(L, R);
   ref<Expr> RShift = LShrExpr::create(Result, R);
   return EqExpr::create(RShift, L);
}

ref<Expr> ExprBuilder::lshrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = LShrExpr::create(L, R);
   ref<Expr> LShift = ShlExpr::create(Result, R);
   return EqExpr::create(LShift, L);
}

ref<Expr> ExprBuilder::ashrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = AShrExpr::create(L, R);
   ref<Expr> LShift = ShlExpr::create(Result, R);
   return EqExpr::create(LShift, L);
}

ref<Expr> ExprBuilder::buildAssoc(
    std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
    llvm::ArrayRef<Inst *> Ops) {
  ref<Expr> E = get(Ops[0]);
  for (Inst *I : llvm::ArrayRef<Inst *>(Ops.data()+1, Ops.size()-1)) {
    E = F(E, get(I));
  }
  return E;
}

ref<Expr> ExprBuilder::countOnes(ref<Expr> L) {
   Expr::Width Width = L->getWidth();
   ref<Expr> Count =  klee::ConstantExpr::create(0, Width);
   for (unsigned i=0; i<Width; i++) {
     ref<Expr> Bit = ExtractExpr::create(L, i, Expr::Bool);
     ref<Expr> BitExt = ZExtExpr::create(Bit, Width);
     Count = AddExpr::create(Count, BitExt);
   }
   return Count;
}

ref<Expr> ExprBuilder::build(Inst *I) {
  const std::vector<Inst *> &Ops = I->orderedOps();
  switch (I->K) {
  case Inst::UntypedConst:
    assert(0 && "unexpected kind");
  case Inst::Const:
    return klee::ConstantExpr::alloc(I->Val);
  case Inst::Var:
    return makeSizedArrayRead(I->Width, I->Name, I);
  case Inst::Phi: {
    const auto &PredExpr = getBlockPredicates(I);
    ref<Expr> E = get(Ops[0]);
    // e.g. P2 ? (P1 ? Op1_Expr : Op2_Expr) : Op3_Expr
    for (unsigned J = 1; J < Ops.size(); ++J) {
      E = SelectExpr::create(PredExpr[J-1], E, get(Ops[J]));
    }
    PhiInsts.push_back(I);
    return E;
  }
  case Inst::Add:
    return buildAssoc(AddExpr::create, Ops);
  case Inst::AddNSW: {
    ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = addnswUB(I);
    return Add;
  }
  case Inst::AddNUW: {
    ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = addnuwUB(I);
    return Add;
  }
  case Inst::AddNW: {
    ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(addnswUB(I), addnuwUB(I));
    return Add;
  }
  case Inst::Sub:
    return SubExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::SubNSW: {
    ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = subnswUB(I);
    return Sub;
  }
  case Inst::SubNUW: {
    ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = subnuwUB(I);
    return Sub;
  }
  case Inst::SubNW: {
    ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(subnswUB(I), subnuwUB(I));
    return Sub;
  }
  case Inst::Mul:
    return buildAssoc(MulExpr::create, Ops);
  case Inst::MulNSW: {
    ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = mulnswUB(I);
    return Mul;
  }
  case Inst::MulNUW: {
    ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = mulnuwUB(I);
    return Mul;
  }
  case Inst::MulNW: {
    ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(mulnswUB(I), mulnuwUB(I));
    return Mul;
  }
  case Inst::UDiv: {
    ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = udivUB(I);
    return Udiv;
  }
  case Inst::SDiv: {
    ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = sdivUB(I);
    return Sdiv;
  }
  case Inst::UDivExact: {
    ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(udivUB(I), udivExactUB(I));
    return Udiv;
  }
  case Inst::SDivExact: {
    ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(sdivUB(I), sdivExactUB(I));
    return Sdiv;
  }
  case Inst::URem: {
    ref<Expr> Urem = URemExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = udivUB(I);
    return Urem;
  }
  case Inst::SRem: {
    ref<Expr> Srem = SRemExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = sdivUB(I);
    return Srem;
  }
  case Inst::And:
    return buildAssoc(AndExpr::create, Ops);
  case Inst::Or:
    return buildAssoc(OrExpr::create, Ops);
  case Inst::Xor:
    return buildAssoc(XorExpr::create, Ops);
  case Inst::Shl: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = shiftUB(I);
    return Result;
  }
  case Inst::ShlNSW: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(shiftUB(I), shlnswUB(I));
    return Result;
  }
  case Inst::ShlNUW: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(shiftUB(I), shlnuwUB(I));
    return Result;
  }
  case Inst::ShlNW: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(shiftUB(I), AndExpr::create(shlnswUB(I), shlnuwUB(I)));
    return Result;
  }
  case Inst::LShr: {
    ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = shiftUB(I);
    return Result;
  }
  case Inst::LShrExact: {
    ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(shiftUB(I), lshrExactUB(I));
    return Result;
  }
  case Inst::AShr: {
    ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = shiftUB(I);
    return Result;
  }
  case Inst::AShrExact: {
    ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
    UBExprMap[I] = AndExpr::create(shiftUB(I), ashrExactUB(I));
    return Result;
  }
  case Inst::Select:
    return SelectExpr::create(get(Ops[0]), get(Ops[1]), get(Ops[2]));
  case Inst::ZExt:
    return ZExtExpr::create(get(Ops[0]), I->Width);
  case Inst::SExt:
    return SExtExpr::create(get(Ops[0]), I->Width);
  case Inst::Trunc:
    return ExtractExpr::create(get(Ops[0]), 0, I->Width);
  case Inst::Eq:
    return EqExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Ne:
    return NeExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Ult:
    return UltExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Slt:
    return SltExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Ule:
    return UleExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Sle:
    return SleExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::CtPop:
    return countOnes(get(Ops[0]));
  case Inst::BSwap: {
    ref<Expr> L = get(Ops[0]);
    unsigned Width = L->getWidth();
    if (Width == 16) {
      ref<Expr> shl1 = ShlExpr::create(L, klee::ConstantExpr::create(8, Width));
      ref<Expr> shr1 = LShrExpr::create(L, klee::ConstantExpr::create(8, Width));
      return OrExpr::create(shl1, shr1);
    }
    else if (Width == 32) {
      ref<Expr> shl1 = ShlExpr::create(L, klee::ConstantExpr::create(24, Width));
      ref<Expr> shl2 = AndExpr::create(ShlExpr::create(L,
                                klee::ConstantExpr::create(8, Width)),
                                klee::ConstantExpr::create(0xFF0000, Width));
      ref<Expr> shr1 = LShrExpr::create(L, klee::ConstantExpr::create(24, Width));
      ref<Expr> shr2 = AndExpr::create(LShrExpr::create(L,
                                klee::ConstantExpr::create(8, Width)),
                                klee::ConstantExpr::create(0xFF00, Width));
      return OrExpr::create(OrExpr::create(shl1, shl2),
                     OrExpr::create(shr1, shr2));
    }
    else if (Width == 64) {
      ref<Expr> shl1 = ShlExpr::create(L, klee::ConstantExpr::create(56, Width));
      ref<Expr> shl2 = AndExpr::create(ShlExpr::create(L,
                                klee::ConstantExpr::create(40, Width)),
                                klee::ConstantExpr::create(0xFF000000000000, Width));
      ref<Expr> shl3 = AndExpr::create(ShlExpr::create(L, klee::ConstantExpr::create(24, Width)),
                                klee::ConstantExpr::create(0xFF0000000000, Width));
      ref<Expr> shl4 = AndExpr::create(ShlExpr::create(L, klee::ConstantExpr::create(8, Width)),
                                klee::ConstantExpr::create(0xFF00000000, Width));
      ref<Expr> shr1 = LShrExpr::create(L, klee::ConstantExpr::create(56, Width));
      ref<Expr> shr2 = AndExpr::create(LShrExpr::create(L, klee::ConstantExpr::create(40, Width)),
                                klee::ConstantExpr::create(0xFF00, Width));
      ref<Expr> shr3 = AndExpr::create(LShrExpr::create(L, klee::ConstantExpr::create(24, Width)),
                                klee::ConstantExpr::create(0xFF0000, Width));
      ref<Expr> shr4 = AndExpr::create(LShrExpr::create(L, klee::ConstantExpr::create(8, Width)),
                                klee::ConstantExpr::create(0xFF000000, Width));
      return OrExpr::create(OrExpr::create(OrExpr::create(
                     shl1, shl2), OrExpr::create(shl3, shl4)), OrExpr::create(
                     OrExpr::create(shr1, shr2), OrExpr::create(shr3, shr4)));
    }
    else ; /* No other bit Width supported */
  }
  case Inst::Cttz: {
    ref<Expr> L = get(Ops[0]);
    unsigned Width = L->getWidth();
    ref<Expr> Val = L;
    for (unsigned i=0, j=0; j<Width/2; i++) {
      j = 1<<i;
      Val = OrExpr::create(Val, ShlExpr::create(Val,
                    klee::ConstantExpr::create(j, Width)));
    }
    return SubExpr::create(klee::ConstantExpr::create(Width, Width),
                    countOnes(Val));
  }
  case Inst::Ctlz: {
    ref<Expr> L = get(Ops[0]);
    unsigned Width = L->getWidth();
    ref<Expr> Val = L;
    for (unsigned i=0, j=0; j<Width/2; i++) {
      j = 1<<i;
      Val = OrExpr::create(Val, LShrExpr::create(Val,
                    klee::ConstantExpr::create(j, Width)));
    }
    return SubExpr::create(klee::ConstantExpr::create(Width, Width),
                    countOnes(Val));
  }
  }
  llvm_unreachable("unknown kind");
}

ref<Expr> ExprBuilder::get(Inst *I) {
  ref<Expr> &E = ExprMap[I];
  if (E.isNull()) {
    E = build(I);
  }
  return E;
}

ref<Expr> ExprBuilder::getInstMapping(const InstMapping &IM) {
  return EqExpr::create(get(IM.Source), get(IM.Replacement));
}

std::vector<ref<Expr> > ExprBuilder::getBlockPredicates(Inst *I) {
  assert(I->K == Inst::Phi && "not a phi inst");
  if (BlockPredMap.count(I->B))
    return BlockPredMap[I->B];
  std::vector<ref<Expr> > PredExpr;
  const std::vector<Inst *> &Ops = I->orderedOps();
  for (unsigned J = 0; J < Ops.size()-1; ++J)
    PredExpr.push_back(makeSizedArrayRead(1, "blockpred", 0));
  BlockPredMap[I->B] = PredExpr;
  return PredExpr;
}

ref<Expr> ExprBuilder::getUBInstCondition() {
  std::set<Inst *> UsedPhis;
  std::set<Inst *> UsedUBInsts;
  ref<Expr> Result = klee::ConstantExpr::create(1, Expr::Bool);
  // For each Phi instruction
  for (const auto &I : PhiInsts) {
    // Skip already processed Phi nodes
    if (UsedPhis.count(I))
      continue;
    // Recursively collect UB instructions
    // on the block constrained Phi branches
    std::vector<std::unique_ptr<PhiPath>> PhiPaths;
    PhiPath *Current = new PhiPath;
    PhiPaths.push_back(std::move(std::unique_ptr<PhiPath>(Current)));
    getUBPhiPaths(I, Current, PhiPaths);
    // For each found path
    for (const auto &Path : PhiPaths) {
      if (!Path->UBInsts.size())
        continue;
      // Aggregate collected UB constraints
      ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
      for (const auto &I : Path->UBInsts) {
        Ante = AndExpr::create(Ante, UBExprMap[I]);
        UsedUBInsts.insert(I);
      }
      // Create path predicate
      ref<Expr> Pred = klee::ConstantExpr::alloc(1, 1);
      for (const auto &Phi : Path->Phis) {
        unsigned Num = Path->BlockConstraints[Phi->B];
        const auto &PredExpr = BlockPredMap[Phi->B];
        // Sanity checks
        assert(PredExpr.size() && "there must be path predicates for the UBs");
        assert(PredExpr.size() == Phi->Ops.size()-1 && "phi predicate size mismatch");
        // Add the predicate(s)
        if (Num == 0)
          Pred = AndExpr::create(Pred, PredExpr[0]);
        else
          Pred = AndExpr::create(Pred, Expr::createIsZero(PredExpr[Num-1]));
        for (unsigned B = Num; B < PredExpr.size(); ++B)
          Pred = AndExpr::create(Pred, PredExpr[B]);
        // Save to processed Phis
        UsedPhis.insert(Phi);
      }
      // Add predicate->UB constraint
      Result = AndExpr::create(Result, Expr::createImplies(Pred, Ante));
    }
  }
  // Add the unconditional UB constraints at the top level
  for (const auto &Entry: UBExprMap)
    if (!UsedUBInsts.count(Entry.first))
      Result = AndExpr::create(Result, Entry.second);
  return Result;
}

void ExprBuilder::getUBPhiPaths(Inst *I, PhiPath *Current,
                                std::vector<std::unique_ptr<PhiPath>> &Paths) {
  switch (I->K) {
    default:
      break;

    case Inst::AddNSW:
    case Inst::AddNUW:
    case Inst::AddNW:
    case Inst::SubNSW:
    case Inst::SubNUW:
    case Inst::SubNW:
    case Inst::MulNSW:
    case Inst::MulNUW:
    case Inst::MulNW:
    case Inst::UDiv:
    case Inst::SDiv:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::URem:
    case Inst::SRem:
    case Inst::Shl:
    case Inst::ShlNSW:
    case Inst::ShlNUW:
    case Inst::ShlNW:
    case Inst::LShr:
    case Inst::LShrExact:
    case Inst::AShr:
    case Inst::AShrExact:
      Current->UBInsts.push_back(I);
      break;
  }

  const std::vector<Inst *> &Ops = I->orderedOps();
  if (I->K == Inst::Phi) {
    Current->Phis.push_back(I);
    if (Current->BlockConstraints.count(I->B)) {
      getUBPhiPaths(Ops[Current->BlockConstraints[I->B]], Current, Paths);
    } else { // Split the context
      std::vector<PhiPath *> Tmp = { Current };
      // Create copies of the current path
      for (unsigned J = 1; J < Ops.size(); ++J) {
        PhiPath *New = new PhiPath;
        *New = *Current;
        New->BlockConstraints[I->B] = J;
        Paths.push_back(std::move(std::unique_ptr<PhiPath>(New)));
        Tmp.push_back(New);
      }
      // Original path takes the first branch
      Current->BlockConstraints[I->B] = 0;
      // Continue recursively
      for (unsigned J = 0; J < Ops.size(); ++J)
        getUBPhiPaths(Ops[J], Tmp[J], Paths);
    }
  } else {
    for (unsigned J = 0; J < Ops.size(); ++J)
      getUBPhiPaths(Ops[J], Current, Paths);
  }
}

// Return an expression which must be proven valid for the candidate to apply.
CandidateExpr souper::GetCandidateExprForReplacement(
    const std::vector<InstMapping> &PCs, InstMapping Mapping) {
  CandidateExpr CE;
  ExprBuilder EB(CE.Arrays, CE.ArrayVars);

  ref<Expr> Cons = EB.getInstMapping(Mapping);
  ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
  for (const auto &PC : PCs) {
    Ante = AndExpr::create(Ante, EB.getInstMapping(PC));
  }
  Ante = AndExpr::create(Ante, EB.getUBInstCondition());

  CE.E = Expr::createImplies(Ante, Cons);

  return CE;
}

namespace {

typedef std::set<std::pair<const Array *, uint64_t>> OffsetSet;

bool IsPurelySymbolic(ref<Expr> E, OffsetSet &Offsets) {
  if (auto RE = dyn_cast<ReadExpr>(E)) {
    auto Offset = dyn_cast<klee::ConstantExpr>(RE->index);
    if (!Offset)
      return false;
    // Check whether we're reading from this array/offset more than once.
    // Such expressions are internally constrained and may be unsat.
    return Offsets.insert(std::make_pair(RE->updates.root,
                                         Offset->getZExtValue())).second;
  }
  else if (auto CE = dyn_cast<ConcatExpr>(E))
    return IsPurelySymbolic(CE->getLeft(), Offsets) &&
           IsPurelySymbolic(CE->getRight(), Offsets);
  else if (auto EE = dyn_cast<ExtractExpr>(E))
    return IsPurelySymbolic(EE->expr, Offsets);
  else
    return false;
}

bool IsPurelySymbolic(ref<Expr> E) {
  OffsetSet Offsets;
  return IsPurelySymbolic(E, Offsets);
}

}

bool souper::IsTriviallyInvalid(ref<Expr> E) {
  // !E => E.
  if (auto EE = dyn_cast<EqExpr>(E)) {
    if (EE->left->getWidth() == 1) {
      if (EE->left->isZero())
        E = EE->right;
      else if (EE->right->isZero())
        E = EE->left;
    }
  }

  if (IsPurelySymbolic(E))
    return true;

  if (auto CE = dyn_cast<CmpExpr>(E)) {
    // e.g. x u< 0 or x s< INT_MIN. TODO: check for these.
    if (isa<UltExpr>(CE) || isa<SltExpr>(CE))
      return false;
    if (isa<klee::ConstantExpr>(CE->left) && IsPurelySymbolic(CE->right))
      return true;
    if (isa<klee::ConstantExpr>(CE->right) && IsPurelySymbolic(CE->left))
      return true;
  }

  return false;
}

std::string souper::BuildQuery(const std::vector<InstMapping> &PCs,
                               InstMapping Mapping,
                               std::vector<Inst *> *ModelVars) {
  std::ostringstream SMTSS;
  ConstraintManager Manager;
  CandidateExpr CE = GetCandidateExprForReplacement(PCs, Mapping);
  Query KQuery(Manager, CE.E);
  ExprSMTLIBLetPrinter Printer;
  Printer.setOutput(SMTSS);
  Printer.setQuery(KQuery);
  std::vector<const klee::Array *> Arrays;
  if (ModelVars) {
    for (unsigned I = 0; I != CE.ArrayVars.size(); ++I) {
      if (CE.ArrayVars[I]) {
        Arrays.push_back(CE.Arrays[I].get());
        ModelVars->push_back(CE.ArrayVars[I]);
      }
    }
    Printer.setArrayValuesToGet(Arrays);
  }
  Printer.generateOutput();

  if (DumpKLEEExprs) {
    SMTSS << "; KLEE expression:\n; ";
    std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SMTSS));
    PP->setForceNoLineBreaks(true);
    PP->scan(CE.E);
    PP->print(CE.E);
    SMTSS << std::endl;
  }

  return SMTSS.str();
}
