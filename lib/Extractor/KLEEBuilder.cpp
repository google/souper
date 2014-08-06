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

struct ExprBuilder {
  ExprBuilder(std::vector<std::unique_ptr<Array>> &Arrays)
      : Arrays(Arrays),
        InstCondition(klee::ConstantExpr::create(1, Expr::Bool)) {}

  std::map<Inst *, ref<Expr> > ExprMap;
  std::map<Inst *, ref<Expr> > UBExprMap;
  std::map<Inst *, std::vector<ref<Expr> >> PhiPredMap;
  std::vector<std::unique_ptr<Array>> &Arrays;
  UniqueNameSet ArrayNames;
  ref<Expr> InstCondition;

  ref<Expr> makeSizedArrayRead(unsigned Width, StringRef Name);
  void addInstCondition(ref<Expr> E);
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
  ref<Expr> buildAssoc(std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
                       llvm::ArrayRef<Inst *> Ops);
  ref<Expr> build(Inst *I);
  ref<Expr> get(Inst *I);
  ref<Expr> getInstMapping(const InstMapping &IM);
  std::vector<ref<Expr >> getPhiPredicates(Inst *I);
  ref<Expr> getUBInstCondition();
  void getUBInstructions(Inst *I, std::vector<Inst *> &Insts);
};

}

ref<Expr> ExprBuilder::makeSizedArrayRead(unsigned Width, StringRef Name) {
  if (Name.empty())
    Name = "arr";
  Arrays.emplace_back(
      new Array(ArrayNames.makeName(Name), 1, 0, 0, Expr::Int32, Width));

  UpdateList UL(Arrays.back().get(), 0);
  return ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
}

void ExprBuilder::addInstCondition(ref<Expr> E) {
  InstCondition = AndExpr::create(InstCondition, E);
}

ref<Expr> ExprBuilder::addnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Add = AddExpr::create(L, R);
   Expr::Width width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, width-1, Expr::Bool);
   ref<Expr> AddMSB = ExtractExpr::create(Add, width-1, Expr::Bool);
   return Expr::createImplies(EqExpr::create(LMSB, RMSB),
                              EqExpr::create(LMSB, AddMSB));
}

ref<Expr> ExprBuilder::addnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, width+1);
   ref<Expr> Rext = ZExtExpr::create(R, width+1);
   ref<Expr> Add = AddExpr::create(Lext, Rext);
   ref<Expr> AddMSB = ExtractExpr::create(Add, width, Expr::Bool);
   return Expr::createIsZero(AddMSB);
}

ref<Expr> ExprBuilder::subnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Sub = SubExpr::create(L, R);
   Expr::Width width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, width-1, Expr::Bool);
   ref<Expr> SubMSB = ExtractExpr::create(Sub, width-1, Expr::Bool);
   return Expr::createImplies(NeExpr::create(LMSB, RMSB),
                              EqExpr::create(LMSB, SubMSB));
}

ref<Expr> ExprBuilder::subnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, width+1);
   ref<Expr> Rext = ZExtExpr::create(R, width+1);
   ref<Expr> Sub = SubExpr::create(Lext, Rext);
   ref<Expr> SubMSB = ExtractExpr::create(Sub, width, Expr::Bool);
   return Expr::createIsZero(SubMSB);
}

ref<Expr> ExprBuilder::mulnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, width-1, Expr::Bool);
   ref<Expr> Signbit = XorExpr::create(LMSB, RMSB);
   ref<Expr> SignbitExt = SExtExpr::create(Signbit, width);
   ref<Expr> Lext = SExtExpr::create(L, 2*width);
   ref<Expr> Rext = SExtExpr::create(R, 2*width);
   ref<Expr> Mul = MulExpr::create(Lext, Rext);
   ref<Expr> Truncated = ExtractExpr::create(Mul, width, width);
   return EqExpr::create(SignbitExt, Truncated);
}

ref<Expr> ExprBuilder::mulnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, 2*width);
   ref<Expr> Rext = ZExtExpr::create(R, 2*width);
   ref<Expr> Mul = MulExpr::create(Lext, Rext);
   ref<Expr> Truncated = ExtractExpr::create(Mul, width, width);
   return Expr::createIsZero(Truncated);
}

ref<Expr> ExprBuilder::udivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
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
   ref<Expr> width = klee::ConstantExpr::create(L->getWidth()-1, L->getWidth());
   ref<Expr> IntMin = ShlExpr::create(klee::ConstantExpr::create(
                                      1, L->getWidth()), width);
   ref<Expr> NegOne = AShrExpr::create(IntMin, width);
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
   unsigned width_value = L->getWidth();
   ref<Expr> Lwidth = klee::ConstantExpr::create(width_value, L->getWidth());
   return UltExpr::create(R, Lwidth);
}

ref<Expr> ExprBuilder::shlnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Lshift_Result = ShlExpr::create(L, R);
   ref<Expr> Rshift_Result = AShrExpr::create(Lshift_Result, R);
   return EqExpr::create(Rshift_Result, L);
}

ref<Expr> ExprBuilder::shlnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Lshift_Result = ShlExpr::create(L, R);
   ref<Expr> Rshift_Result = LShrExpr::create(Lshift_Result, R);
   return EqExpr::create(Rshift_Result, L);
}

ref<Expr> ExprBuilder::lshrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Rshift_Result = LShrExpr::create(L, R);
   ref<Expr> Lshift_Result = ShlExpr::create(Rshift_Result, R);
   return EqExpr::create(Lshift_Result, L);
}

ref<Expr> ExprBuilder::ashrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Rshift_Result = AShrExpr::create(L, R);
   ref<Expr> Lshift_Result = ShlExpr::create(Rshift_Result, R);
   return EqExpr::create(Lshift_Result, L);
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

ref<Expr> ExprBuilder::build(Inst *I) {
  const std::vector<Inst *> &Ops = I->orderedOps();
  switch (I->K) {
  case Inst::Const:
    return klee::ConstantExpr::alloc(I->Val);
  case Inst::Var:
    return makeSizedArrayRead(I->Width, I->Name);
  case Inst::Phi: {
    const auto &PredExpr = getPhiPredicates(I);
    ref<Expr> E = get(Ops[0]);
    // e.g. P2 ? (P1 ? Op1_Expr : Op2_Expr) : Op3_Expr
    for (unsigned J = 1; J < Ops.size(); ++J) {
      E = SelectExpr::create(PredExpr[J-1], E, get(Ops[J]));
    }
    return E;
  }
  case Inst::Add:
    return buildAssoc(AddExpr::create, Ops);
  case Inst::AddNSW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Add = AddExpr::create(L, R);
    UBExprMap[I] = addnswUB(I);
    return Add;
  }
  case Inst::AddNUW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Add = AddExpr::create(L, R);
    UBExprMap[I] = addnuwUB(I);
    return Add;
  }
  case Inst::AddNW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Add = AddExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(addnswUB(I), addnuwUB(I));
    return Add;
  }
  case Inst::Sub:
    return SubExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::SubNSW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Sub = SubExpr::create(L, R);
    UBExprMap[I] = subnswUB(I);
    return Sub;
  }
  case Inst::SubNUW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Sub = SubExpr::create(L, R);
    UBExprMap[I] = subnuwUB(I);
    return Sub;
  }
  case Inst::SubNW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Sub = SubExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(subnswUB(I), subnuwUB(I));
    return Sub;
  }
  case Inst::Mul:
    return buildAssoc(MulExpr::create, Ops);
  case Inst::MulNSW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Mul = MulExpr::create(L, R);
    UBExprMap[I] = mulnswUB(I);
    return Mul;
  }
  case Inst::MulNUW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Mul = MulExpr::create(L, R);
    UBExprMap[I] = mulnuwUB(I);
    return Mul;
  }
  case Inst::MulNW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Mul = MulExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(mulnswUB(I), mulnuwUB(I));
    return Mul;
  }
  case Inst::UDiv: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Udiv = UDivExpr::create(L, R);
    UBExprMap[I] = udivUB(I);
    return Udiv;
  }
  case Inst::SDiv: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Sdiv = SDivExpr::create(L, R);
    UBExprMap[I] = sdivUB(I);
    return Sdiv;
  }
  case Inst::UDivExact: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Udiv = UDivExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(udivUB(I), udivExactUB(I));
    return Udiv;
  }
  case Inst::SDivExact: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Sdiv = SDivExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(sdivUB(I), sdivExactUB(I));
    return Sdiv;
  }
  case Inst::URem: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Urem = URemExpr::create(L, R);
    UBExprMap[I] = udivUB(I);
    return Urem;
  }
  case Inst::SRem: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Srem = SRemExpr::create(L, R);
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
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Result = ShlExpr::create(L, R);
    UBExprMap[I] = shiftUB(I);
    return Result;
  }
  case Inst::ShlNSW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Result = ShlExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(shiftUB(I), shlnswUB(I));
    return Result;
  }
  case Inst::ShlNUW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Result = ShlExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(shiftUB(I), shlnuwUB(I));
    return Result;
  }
  case Inst::ShlNW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Result = ShlExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(shiftUB(I), AndExpr::create(shlnswUB(I), shlnuwUB(I)));
    return Result;
  }
  case Inst::LShr: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Result = LShrExpr::create(L, R);
    UBExprMap[I] = shiftUB(I);
    return Result;
  }
  case Inst::LShrExact: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Rshift_Result = LShrExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(shiftUB(I), lshrExactUB(I));
    return Rshift_Result;
  }
  case Inst::AShr: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Result = AShrExpr::create(L, R);
    UBExprMap[I] = shiftUB(I);
    return Result;
  }
  case Inst::AShrExact: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Rshift_Result = AShrExpr::create(L, R);
    UBExprMap[I] = AndExpr::create(shiftUB(I), ashrExactUB(I));
    return Rshift_Result;
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

ref<Expr> ExprBuilder::getUBInstCondition() {
  ref<Expr> Result = klee::ConstantExpr::create(1, Expr::Bool);
  std::set<Inst *> UsedUBInst;
  // For each Phi node
  for (const auto &V : PhiPredMap) {
    // For each Phi operand
    for (unsigned A = 0; A < V.first->Ops.size(); ++A) {
      std::vector<Inst *> UBInst;
      // Scan for UB instructions
      getUBInstructions(V.first->Ops[A], UBInst);
      if (UBInst.size() > 0) {
        ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
        // Collect UB constraints
        for (Inst *I : UBInst) {
          assert(UBExprMap.count(I) && "UB instruction expression not found");
          UsedUBInst.insert(I);
          Ante = AndExpr::create(Ante, UBExprMap[I]);
        }
        // Get operand predicate
        ref<Expr> Pred;
        if (A == 0)
          Pred = V.second[0];
        else
          Pred = Expr::createIsZero(V.second[A-1]);
        // Add predicate->UB constraint
        Result = Expr::createImplies(Pred, Ante);
      }
    }
  }
  // Add the remaining UB constraints
  for (const auto &Entry: UBExprMap) {
    if (!UsedUBInst.count(Entry.first)) {
      Result = AndExpr::create(Result, Entry.second);
    }
  }
  return Result;
}

std::vector<ref<Expr> > ExprBuilder::getPhiPredicates(Inst *I) {
  assert(!PhiPredMap.count(I) && "phi predicates exist");
  const std::vector<Inst *> &Ops = I->orderedOps();
  std::vector<ref<Expr> > PredExpr;
  for (unsigned J = 0; J < Ops.size()-1; ++J)
    PredExpr.push_back(makeSizedArrayRead(1, "blockpred"));
  PhiPredMap[I] = PredExpr;
  return PredExpr;
}

void ExprBuilder::getUBInstructions(Inst *I, std::vector<Inst *> &Insts) {
  switch (I->K) {
    default:
      break;

    case Inst::Const:
      break;

    case Inst::Phi:
      break;

    case Inst::AddNSW:
    case Inst::AddNUW:
    case Inst::AddNW:
    case Inst::SubNSW:
    case Inst::SubNUW:
    case Inst::SubNW:
    case Inst::MulNSW:
    case Inst::MulNUW:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::MulNW:
    case Inst::ShlNSW:
    case Inst::ShlNUW:
    case Inst::ShlNW:
    case Inst::LShrExact:
    case Inst::AShrExact:
      Insts.push_back(I);
      break;
  }

  const std::vector<Inst *> &Ops = I->orderedOps();
  for (unsigned J = 0; J != Ops.size(); ++J) {
    getUBInstructions(Ops[J], Insts);
  }
}

// Return an expression which must be proven valid for the candidate to apply.
CandidateExpr souper::GetCandidateExprForReplacement(
    const std::vector<InstMapping> &PCs, InstMapping Mapping) {
  CandidateExpr CE;
  ExprBuilder EB(CE.Arrays);

  ref<Expr> Cons = EB.getInstMapping(Mapping);
  ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
  for (const auto &PC : PCs) {
    Ante = AndExpr::create(Ante, EB.getInstMapping(PC));
  }
  Ante = AndExpr::create(Ante, EB.InstCondition);

  CE.E = Expr::createImplies(Ante, Cons);

  // Add top level UB constraints
  CE.E = AndExpr::create(CE.E, EB.getUBInstCondition());

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
                               InstMapping Mapping) {
  std::ostringstream SMTSS;
  ConstraintManager Manager;
  CandidateExpr CE = GetCandidateExprForReplacement(PCs, Mapping);
  Query KQuery(Manager, CE.E);
  ExprSMTLIBLetPrinter Printer;
  Printer.setOutput(SMTSS);
  Printer.setQuery(KQuery);
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
