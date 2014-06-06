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

#include <functional>
#include <map>
#include <memory>
#include <sstream>

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CommandLine.h"
#include "klee/Expr.h"
#include "klee/Solver.h"
#include "klee/util/Ref.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/util/ExprSMTLIBLetPrinter.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"

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
  std::vector<std::unique_ptr<Array>> &Arrays;
  UniqueNameSet ArrayNames;
  ref<Expr> InstCondition;

  ref<Expr> makeSizedArrayRead(unsigned Width, StringRef Name);
  void addInstCondition(ref<Expr> E);
  ref<Expr> buildAssoc(std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
                       llvm::ArrayRef<Inst *> Ops);
  ref<Expr> build(Inst *I);
  ref<Expr> get(Inst *I);
  ref<Expr> getInstMapping(const InstMapping &IM);
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
  case Inst::Phi:
    return buildAssoc([&](ref<Expr> L, ref<Expr> R) {
                        return SelectExpr::create(
                            makeSizedArrayRead(1, "blockpred"), L, R);
                      },
                      Ops);
  case Inst::Add:
    return buildAssoc(AddExpr::create, Ops);
  case Inst::AddNSW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Add = AddExpr::create(L, R);
    ref<Expr> LMSB = ExtractExpr::create(L, I->Width-1, Expr::Bool);
    ref<Expr> RMSB = ExtractExpr::create(R, I->Width-1, Expr::Bool);
    ref<Expr> AddMSB = ExtractExpr::create(Add, I->Width-1, Expr::Bool);
    addInstCondition(Expr::createImplies(EqExpr::create(LMSB, RMSB),
                                         EqExpr::create(LMSB, AddMSB)));
    return Add;
  }
  case Inst::Sub:
    return SubExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::SubNSW: {
    ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
    ref<Expr> Sub = SubExpr::create(L, R);
    ref<Expr> LMSB = ExtractExpr::create(L, I->Width-1, Expr::Bool);
    ref<Expr> RMSB = ExtractExpr::create(R, I->Width-1, Expr::Bool);
    ref<Expr> SubMSB = ExtractExpr::create(Sub, I->Width-1, Expr::Bool);
    addInstCondition(Expr::createImplies(NeExpr::create(LMSB, RMSB),
                                         EqExpr::create(LMSB, SubMSB)));
    return Sub;
  }
  case Inst::Mul:
    return buildAssoc(MulExpr::create, Ops);
  case Inst::UDiv:
    return UDivExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::SDiv:
    return SDivExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::URem:
    return URemExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::SRem:
    return SRemExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::And:
    return buildAssoc(AndExpr::create, Ops);
  case Inst::Or:
    return buildAssoc(OrExpr::create, Ops);
  case Inst::Xor:
    return buildAssoc(XorExpr::create, Ops);
  case Inst::Shl:
    return ShlExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::LShr:
    return LShrExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::AShr:
    return AShrExpr::create(get(Ops[0]), get(Ops[1]));
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

// Return an expression which must be proven valid for the candidate to apply.
CandidateExpr souper::GetCandidateExprForReplacement(
    const std::vector<InstMapping> &PCs,
    InstMapping Mapping) {
  CandidateExpr CE;
  ExprBuilder EB(CE.Arrays);

  ref<Expr> Cons = EB.getInstMapping(Mapping);
  ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
  for (const auto &PC : PCs) {
    Ante = AndExpr::create(Ante, EB.getInstMapping(PC));
  }
  Ante = AndExpr::create(Ante, EB.InstCondition);

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
