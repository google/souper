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

#include "klee/Expr.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/util/ExprSMTLIBPrinter.h"
#include "klee/util/Ref.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Extractor/ExprBuilder.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <functional>
#include <map>
#include <memory>
#include <sstream>
#include <unordered_map>

static llvm::cl::opt<bool> ExploitUB(
    "souper-exploit-ub",
    llvm::cl::desc("Exploit undefined behavior (default=true)"),
    llvm::cl::init(true));
static llvm::cl::opt<bool> DumpKLEEExprs(
    "dump-klee-exprs",
    llvm::cl::desc("Dump KLEE expressions after SMTLIB queries"),
    llvm::cl::init(false));

using namespace llvm;
using namespace klee;
using namespace souper;

namespace {

class KLEEBuilder : public ExprBuilder {
public:
  KLEEBuilder(InstContext &IC) {
    LIC = &IC;
  }
  ~KLEEBuilder() {}

  //TODO
  std::map<Inst *, ref<Expr>> ExprMap;

  // Return an expression which must be proven valid for the candidate to apply.
  llvm::Optional<CandidateExpr> GetCandidateExprForReplacement(
      const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
      InstMapping Mapping, bool Negate) override {
  
    // Build LHS
    ref<Expr> LHS = get(Mapping.LHS);
    ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);

    // Get demanded bits constraints
    ref<Expr> DemandedBits = klee::ConstantExpr::alloc(Mapping.LHS->DemandedBits);
    if (!Mapping.LHS->DemandedBits.isAllOnesValue())
      LHS = AndExpr::create(LHS, DemandedBits);
    for (const auto I : CE.Vars) {
      if (I)
        Ante = AndExpr::create(Ante, get(getDemandedBitsCondition(I)));
    }

    // Build PCs
    for (const auto &PC : PCs) {
      Ante = AndExpr::create(Ante, get(getInstMapping(PC)));
    }
    // Build BPCs
    if (BPCs.size()) {
      setBlockPCMap(BPCs);
      Ante = AndExpr::create(Ante, get(getBlockPCs()));
    }
    // Get UB constraints of LHS and (B)PCs
    ref<Expr> LHSPCsUB = klee::ConstantExpr::create(1, Expr::Bool);
    if (ExploitUB) {
      LHSPCsUB = get(getUBInstCondition());
      if (LHSPCsUB.isNull())
        return llvm::Optional<CandidateExpr>();
    }
    // Build RHS
    ref<Expr> RHS = get(Mapping.RHS);
    if (!Mapping.LHS->DemandedBits.isAllOnesValue())
      RHS = AndExpr::create(RHS, DemandedBits);
    // Get all UB constraints (LHS && (B)PCs && RHS)
    ref<Expr> UB = klee::ConstantExpr::create(1, Expr::Bool);
    if (ExploitUB) {
      UB = get(getUBInstCondition());
      if (UB.isNull())
        return llvm::Optional<CandidateExpr>();
    }
  
    ref<Expr> Cons;
    if (Negate) // (LHS != RHS)
      Cons = NeExpr::create(LHS, RHS);
    else        // (LHS == RHS)
      Cons = EqExpr::create(LHS, RHS);
    // Cons && UB
    if (Mapping.RHS->K != Inst::Const)
      Cons = AndExpr::create(Cons, UB);
    // (LHS UB && (B)PCs && (B)PCs UB)
    Ante = AndExpr::create(Ante, LHSPCsUB);
    // (LHS UB && (B)PCs && (B)PCs UB) => Cons && UB
    CE.E = Expr::createImplies(Ante, Cons);
  
    return llvm::Optional<CandidateExpr>(std::move(CE));
  }

  std::string BuildQuery(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars, bool Negate) override {
    std::string SMTStr;
    llvm::raw_string_ostream SMTSS(SMTStr);
    ConstraintManager Manager;
    Optional<CandidateExpr> OptionalCE = GetCandidateExprForReplacement(BPCs,
        PCs, Mapping, Negate);
    if (!OptionalCE.hasValue())
      return std::string();
    CandidateExpr CE = std::move(OptionalCE.getValue());
    ref<Expr> E = CE.E;
    Query KQuery(Manager, E);
    ExprSMTLIBPrinter Printer;
    Printer.setOutput(SMTSS);
    Printer.setQuery(KQuery);
    std::vector<const klee::Array *> Arrays;
    if (ModelVars) {
      for (unsigned I = 0; I != CE.Vars.size(); ++I) {
        if (CE.Vars[I]) {
          Arrays.push_back(CE.Arrays[I].get());
          ModelVars->push_back(CE.Vars[I]);
        }
      }
      Printer.setArrayValuesToGet(Arrays);
    }
    Printer.generateOutput();
  
    if (DumpKLEEExprs) {
      SMTSS << "; KLEE expression:\n; ";
      std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SMTSS));
      PP->setForceNoLineBreaks(true);
      PP->scan(E);
      PP->print(E);
      SMTSS << '\n';
    }
  
    return SMTSS.str();
  }

private:
  ref<Expr> countOnes(ref<Expr> L) {
     Expr::Width Width = L->getWidth();
     ref<Expr> Count =  klee::ConstantExpr::alloc(llvm::APInt(Width, 0));
     for (unsigned i=0; i<Width; i++) {
       ref<Expr> Bit = ExtractExpr::create(L, i, Expr::Bool);
       ref<Expr> BitExt = ZExtExpr::create(Bit, Width);
       Count = AddExpr::create(Count, BitExt);
     }
     return Count;
  }

  ref<Expr> buildAssoc(
      std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
      llvm::ArrayRef<Inst *> Ops) {
    ref<Expr> E = get(Ops[0]);
    for (Inst *I : llvm::ArrayRef<Inst *>(Ops.data()+1, Ops.size()-1)) {
      E = F(E, get(I));
    }
    return E;
  }

  ref<Expr> build(Inst *I) {
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
        E = SelectExpr::create(get(PredExpr[J-1]), E, get(Ops[J]));
      }
      UBPathInsts.push_back(I);
      return E;
    }
    case Inst::Add:
      return buildAssoc(AddExpr::create, Ops);
    case Inst::AddNSW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, addnswUB(I));
      return Add;
    }
    case Inst::AddNUW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, addnuwUB(I));
      return Add;
    }
    case Inst::AddNW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                          {addnswUB(I), addnuwUB(I)}));
      return Add;
    }
    case Inst::Sub:
      return SubExpr::create(get(Ops[0]), get(Ops[1]));
    case Inst::SubNSW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, subnswUB(I));
      return Sub;
    }
    case Inst::SubNUW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, subnuwUB(I));
      return Sub;
    }
    case Inst::SubNW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                          {subnswUB(I), subnuwUB(I)}));
      return Sub;
    }
    case Inst::Mul:
      return buildAssoc(MulExpr::create, Ops);
    case Inst::MulNSW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, mulnswUB(I));
      return Mul;
    }
    case Inst::MulNUW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, mulnuwUB(I));
      return Mul;
    }
    case Inst::MulNW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                          {mulnswUB(I), mulnuwUB(I)}));
      return Mul;
    }
  
    // We introduce these extra checks here because KLEE invokes llvm::APInt's
    // div functions , which crash upon divide-by-zero.
    case Inst::UDiv:
    case Inst::SDiv:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::URem:
    case Inst::SRem: { // Fall-through
      // If the second oprand is 0, then it definitely causes UB.
      // There are quite a few cases where KLEE folds operations into zero,
      // e.g., "sext i16 0 to i32", "0 + 0", "2 - 2", etc.  In all cases,
      // we skip building the corresponding KLEE expressions and just return
      // a constant zero.
      ref<Expr> R = get(Ops[1]);
      if (R->isZero()) {
        recordUBInstruction(I, LIC->getConst(APInt(1, false)));
        return klee::ConstantExpr::create(0, Ops[1]->Width);
      }
  
      switch (I->K) {
      default:
        break;
  
      case Inst::UDiv: {
        ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        recordUBInstruction(I, udivUB(I));
        return Udiv;
      }
      case Inst::SDiv: {
        ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        recordUBInstruction(I, sdivUB(I));
        return Sdiv;
      }
      case Inst::UDivExact: {
        ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                            {udivUB(I), udivExactUB(I)}));
        return Udiv;
      }
      case Inst::SDivExact: {
        ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        recordUBInstruction(I, LIC->getInst(Inst::And, 1, {sdivUB(I), sdivExactUB(I)}));
        return Sdiv;
      }
      case Inst::URem: {
        ref<Expr> Urem = URemExpr::create(get(Ops[0]), R);
        recordUBInstruction(I, udivUB(I));
        return Urem;
      }
      case Inst::SRem: {
        ref<Expr> Srem = SRemExpr::create(get(Ops[0]), R);
        recordUBInstruction(I, sdivUB(I));
        return Srem;
      }
      llvm_unreachable("unknown kind");
    }
    }
  
    case Inst::And:
      return buildAssoc(AndExpr::create, Ops);
    case Inst::Or:
      return buildAssoc(OrExpr::create, Ops);
    case Inst::Xor:
      return buildAssoc(XorExpr::create, Ops);
    case Inst::Shl: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, shiftUB(I));
      return Result;
    }
    case Inst::ShlNSW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1, {shiftUB(I), shlnswUB(I)}));
      return Result;
    }
    case Inst::ShlNUW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1, {shiftUB(I), shlnuwUB(I)}));
      return Result;
    }
    case Inst::ShlNW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                          {shiftUB(I), LIC->getInst(Inst::And, 1, {shlnswUB(I), shlnuwUB(I)})}));
      return Result;
    }
    case Inst::LShr: {
      ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, shiftUB(I));
      return Result;
    }
    case Inst::LShrExact: {
      ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                          {shiftUB(I), lshrExactUB(I)}));
      return Result;
    }
    case Inst::AShr: {
      ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, shiftUB(I));
      return Result;
    }
    case Inst::AShrExact: {
      ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
      recordUBInstruction(I, LIC->getInst(Inst::And, 1,
                                          {shiftUB(I), ashrExactUB(I)}));
      return Result;
    }
    case Inst::Select:
      UBPathInsts.push_back(I);
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
        return ConcatExpr::create(ExtractExpr::create(L, 0, 8),
                                  ExtractExpr::create(L, 8, 8));
      }
      else if (Width == 32) {
        return ConcatExpr::create4(ExtractExpr::create(L, 0, 8),
                                   ExtractExpr::create(L, 8, 8),
                                   ExtractExpr::create(L, 16, 8),
                                   ExtractExpr::create(L, 24, 8));
      }
      else if (Width == 64) {
        return ConcatExpr::create8(ExtractExpr::create(L, 0, 8),
                                   ExtractExpr::create(L, 8, 8),
                                   ExtractExpr::create(L, 16, 8),
                                   ExtractExpr::create(L, 24, 8),
                                   ExtractExpr::create(L, 32, 8),
                                   ExtractExpr::create(L, 40, 8),
                                   ExtractExpr::create(L, 48, 8),
                                   ExtractExpr::create(L, 56, 8));
      }
      break;
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
    case Inst::SAddO:
      return XorExpr::create(get(addnswUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::UAddO:
      return XorExpr::create(get(addnuwUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::SSubO:
      return XorExpr::create(get(subnswUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::USubO:
      return XorExpr::create(get(subnuwUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::SMulO:
      return XorExpr::create(get(mulnswUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::UMulO:
      return XorExpr::create(get(mulnuwUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::ExtractValue: {
      unsigned Index = Ops[1]->Val.getZExtValue();
      return get(Ops[0]->Ops[Index]);
    }
    case Inst::SAddWithOverflow:
    case Inst::UAddWithOverflow:
    case Inst::SSubWithOverflow:
    case Inst::USubWithOverflow:
    case Inst::SMulWithOverflow:
    case Inst::UMulWithOverflow:
    default:
      break;
    }
    llvm_unreachable("unknown kind");
  }
  
  ref<Expr> get(Inst *I) {
    ref<Expr> &E = ExprMap[I];
    if (E.isNull()) {
      E = build(I);
      assert(E->getWidth() == I->Width);
    }
    return E;
  }
  
  ref<Expr> makeSizedArrayRead(unsigned Width, StringRef Name, Inst *Origin) {
    std::string NameStr;
    if (Name.empty())
      NameStr = "arr";
    else if (Name[0] >= '0' && Name[0] <= '9')
      NameStr = ("a" + Name).str();
    else
      NameStr = Name;
    CE.Arrays.emplace_back(
        new Array(ArrayNames.makeName(NameStr), 1, 0, 0, Expr::Int32, Width));
    CE.Vars.push_back(Origin);
  
    UpdateList UL(CE.Arrays.back().get(), 0);
    return ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
  }

};

}

std::unique_ptr<ExprBuilder> souper::createKLEEBuilder(InstContext &IC) {
  return std::unique_ptr<ExprBuilder>(new KLEEBuilder(IC));
}
