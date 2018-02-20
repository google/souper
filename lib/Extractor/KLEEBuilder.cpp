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

static llvm::cl::opt<bool> DumpKLEEExprs(
    "dump-klee-exprs",
    llvm::cl::desc("Dump KLEE expressions after SMTLIB queries"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> ExploitUB(
    "souper-exploit-ub",
    llvm::cl::desc("Exploit undefined behavior (default=true)"),
    llvm::cl::init(true));

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
    Query KQuery(Manager, CE.E);
    ExprSMTLIBPrinter Printer;
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
      SMTSS << '\n';
    }
  
    return SMTSS.str();
  }

private:
  ref<Expr> addnswUB(Inst *I) override {
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
  
  ref<Expr> addnuwUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     Expr::Width Width = L->getWidth();
     ref<Expr> Lext = ZExtExpr::create(L, Width+1);
     ref<Expr> Rext = ZExtExpr::create(R, Width+1);
     ref<Expr> Add = AddExpr::create(Lext, Rext);
     ref<Expr> AddMSB = ExtractExpr::create(Add, Width, Expr::Bool);
     return Expr::createIsZero(AddMSB);
  }
  
  ref<Expr> subnswUB(Inst *I) override {
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
  
  ref<Expr> subnuwUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     Expr::Width Width = L->getWidth();
     ref<Expr> Lext = ZExtExpr::create(L, Width+1);
     ref<Expr> Rext = ZExtExpr::create(R, Width+1);
     ref<Expr> Sub = SubExpr::create(Lext, Rext);
     ref<Expr> SubMSB = ExtractExpr::create(Sub, Width, Expr::Bool);
     return Expr::createIsZero(SubMSB);
  }
  
  ref<Expr> mulnswUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     // The computation below has to be performed on the operands of
     // multiplication instruction. The instruction using mulnswUB()
     // can be of different width, for instance in SMulO instruction
     // which is of 1-bit, but the operands width are to be used here.
     Expr::Width Width = get(Ops[0])->getWidth();
     ref<Expr> L = SExtExpr::create(get(Ops[0]), 2*Width);
     ref<Expr> R = SExtExpr::create(get(Ops[1]), 2*Width);
     ref<Expr> Mul = MulExpr::create(L, R);
     ref<Expr> LowerBits = ExtractExpr::create(Mul, 0, Width);
     ref<Expr> LowerBitsExt = SExtExpr::create(LowerBits, 2*Width);
     return EqExpr::create(Mul, LowerBitsExt);
  }
  
  ref<Expr> mulnuwUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     Expr::Width Width = L->getWidth();
     ref<Expr> Lext = ZExtExpr::create(L, 2*Width);
     ref<Expr> Rext = ZExtExpr::create(R, 2*Width);
     ref<Expr> Mul = MulExpr::create(Lext, Rext);
     ref<Expr> HigherBits = ExtractExpr::create(Mul, Width, Width);
     return Expr::createIsZero(HigherBits);
  }
  
  ref<Expr> udivUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> R = get(Ops[1]);
     return NeExpr::create(R, klee::ConstantExpr::create(0, R->getWidth()));
  }
  
  ref<Expr> udivExactUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Udiv = UDivExpr::create(L, R);
     return EqExpr::create(L, MulExpr::create(R, Udiv));
  }
  
  ref<Expr> sdivUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> ShiftBy = klee::ConstantExpr::create(L->getWidth()-1,
                                                    L->getWidth());
     ref<Expr> IntMin = ShlExpr::create(klee::ConstantExpr::create(
                                        1, L->getWidth()), ShiftBy);
     ref<Expr> NegOne = AShrExpr::create(IntMin, ShiftBy);
     return AndExpr::create(NeExpr::create(R, klee::ConstantExpr::create(
                            0, R->getWidth())), OrExpr::create(
                            NeExpr::create(L, IntMin), NeExpr::create(R, NegOne)));
  }
  
  ref<Expr> sdivExactUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Sdiv = SDivExpr::create(L, R);
     return EqExpr::create(L, MulExpr::create(R, Sdiv));
  }
  
  ref<Expr> shiftUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Lwidth = klee::ConstantExpr::create(L->getWidth(), L->getWidth());
     return UltExpr::create(R, Lwidth);
  }
  
  ref<Expr> shlnswUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Result = ShlExpr::create(L, R);
     ref<Expr> RShift = AShrExpr::create(Result, R);
     return EqExpr::create(RShift, L);
  }
  
  ref<Expr> shlnuwUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Result = ShlExpr::create(L, R);
     ref<Expr> RShift = LShrExpr::create(Result, R);
     return EqExpr::create(RShift, L);
  }
  
  ref<Expr> lshrExactUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Result = LShrExpr::create(L, R);
     ref<Expr> LShift = ShlExpr::create(Result, R);
     return EqExpr::create(LShift, L);
  }
  
  ref<Expr> ashrExactUB(Inst *I) override {
     const std::vector<Inst *> &Ops = I->orderedOps();
     ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
     ref<Expr> Result = AShrExpr::create(L, R);
     ref<Expr> LShift = ShlExpr::create(Result, R);
     return EqExpr::create(LShift, L);
  }
  
  ref<Expr> countOnes(ref<Expr> L) override {
     Expr::Width Width = L->getWidth();
     ref<Expr> Count =  klee::ConstantExpr::alloc(llvm::APInt(Width, 0));
     for (unsigned i=0; i<Width; i++) {
       ref<Expr> Bit = ExtractExpr::create(L, i, Expr::Bool);
       ref<Expr> BitExt = ZExtExpr::create(Bit, Width);
       Count = AddExpr::create(Count, BitExt);
     }
     return Count;
  }
  
  ref<Expr> build(Inst *I) override {
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
      //recordUBInstruction(I, addnswUB(I));
      return Add;
    }
    case Inst::AddNUW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, addnuwUB(I));
      return Add;
    }
    case Inst::AddNW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(addnswUB(I), addnuwUB(I)));
      return Add;
    }
    case Inst::Sub:
      return SubExpr::create(get(Ops[0]), get(Ops[1]));
    case Inst::SubNSW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, subnswUB(I));
      return Sub;
    }
    case Inst::SubNUW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, subnuwUB(I));
      return Sub;
    }
    case Inst::SubNW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(subnswUB(I), subnuwUB(I)));
      return Sub;
    }
    case Inst::Mul:
      return buildAssoc(MulExpr::create, Ops);
    case Inst::MulNSW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, mulnswUB(I));
      return Mul;
    }
    case Inst::MulNUW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, mulnuwUB(I));
      return Mul;
    }
    case Inst::MulNW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(mulnswUB(I), mulnuwUB(I)));
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
        //recordUBInstruction(I, klee::ConstantExpr::create(0, 1));
        return klee::ConstantExpr::create(0, Ops[1]->Width);
      }
  
      switch (I->K) {
      default:
        break;
  
      case Inst::UDiv: {
        ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        //recordUBInstruction(I, udivUB(I));
        return Udiv;
      }
      case Inst::SDiv: {
        ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        //recordUBInstruction(I, sdivUB(I));
        return Sdiv;
      }
      case Inst::UDivExact: {
        ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        //recordUBInstruction(I, AndExpr::create(udivUB(I), udivExactUB(I)));
        return Udiv;
      }
      case Inst::SDivExact: {
        ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        //recordUBInstruction(I, AndExpr::create(sdivUB(I), sdivExactUB(I)));
        return Sdiv;
      }
      case Inst::URem: {
        ref<Expr> Urem = URemExpr::create(get(Ops[0]), R);
        //recordUBInstruction(I, udivUB(I));
        return Urem;
      }
      case Inst::SRem: {
        ref<Expr> Srem = SRemExpr::create(get(Ops[0]), R);
        //recordUBInstruction(I, sdivUB(I));
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
      //recordUBInstruction(I, shiftUB(I));
      return Result;
    }
    case Inst::ShlNSW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(shiftUB(I), shlnswUB(I)));
      return Result;
    }
    case Inst::ShlNUW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(shiftUB(I), shlnuwUB(I)));
      return Result;
    }
    case Inst::ShlNW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(shiftUB(I),
      //                                       AndExpr::create(shlnswUB(I),
      //                                                       shlnuwUB(I))));
      return Result;
    }
    case Inst::LShr: {
      ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, shiftUB(I));
      return Result;
    }
    case Inst::LShrExact: {
      ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(shiftUB(I), lshrExactUB(I)));
      return Result;
    }
    case Inst::AShr: {
      ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, shiftUB(I));
      return Result;
    }
    case Inst::AShrExact: {
      ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
      //recordUBInstruction(I, AndExpr::create(shiftUB(I), ashrExactUB(I)));
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
      return XorExpr::create(addnswUB(I), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::UAddO:
      return XorExpr::create(addnuwUB(I), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::SSubO:
      return XorExpr::create(subnswUB(I), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::USubO:
      return XorExpr::create(subnuwUB(I), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::SMulO:
      return XorExpr::create(mulnswUB(I), klee::ConstantExpr::create(1, Expr::Bool));
    case Inst::UMulO:
      return XorExpr::create(mulnuwUB(I), klee::ConstantExpr::create(1, Expr::Bool));
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
  
  ref<Expr> get(Inst *I) override {
    ref<Expr> &E = ExprMap[I];
    if (E.isNull()) {
      E = build(I);
      assert(E->getWidth() == I->Width);
    }
    return E;
  }
  
  // Return an expression which must be proven valid for the candidate to apply.
  llvm::Optional<CandidateExpr> GetCandidateExprForReplacement(
      const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
      InstMapping Mapping, bool Negate) override {
    // Build LHS
    ref<Expr> LHS = get(Mapping.LHS);
    ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
    ref<Expr> DemandedBits = klee::ConstantExpr::alloc(Mapping.LHS->DemandedBits);
    if (!Mapping.LHS->DemandedBits.isAllOnesValue())
      LHS = AndExpr::create(LHS, DemandedBits);
    for (const auto I : CE.ArrayVars) {
      if (I) {
        if (I->KnownZeros.getBoolValue() || I->KnownOnes.getBoolValue()) {
          Ante = AndExpr::create(Ante, getZeroBitsMapping(I));
          Ante = AndExpr::create(Ante, getOneBitsMapping(I));
        }
        if (I->NonZero)
          Ante = AndExpr::create(Ante, getNonZeroBitsMapping(I));
        if (I->NonNegative)
          Ante = AndExpr::create(Ante, getNonNegBitsMapping(I));
        if (I->PowOfTwo)
          Ante = AndExpr::create(Ante, getPowerTwoBitsMapping(I));
        if (I->Negative)
          Ante = AndExpr::create(Ante, getNegBitsMapping(I));
        if (I->NumSignBits > 1)
          Ante = AndExpr::create(Ante, getSignBitsMapping(I));
      }
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
    CE.ArrayVars.push_back(Origin);
  
    std::vector<unsigned> ZeroBits, OneBits;
    UpdateList UL(CE.Arrays.back().get(), 0);
    ref<Expr> Var = ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
    if (Origin && Origin->K == Inst::Var) {
      if (Origin->KnownZeros.getBoolValue() || Origin->KnownOnes.getBoolValue()) {
        ref<Expr> NotZeros = NotExpr::create(klee::ConstantExpr::alloc(Origin->KnownZeros));
        ref<Expr> VarOrNotZero = OrExpr::create(Var, NotZeros);
        ZeroBitsMap[Origin] = EqExpr::create(VarOrNotZero, NotZeros);
        ref<Expr> Ones = klee::ConstantExpr::alloc(Origin->KnownOnes);
        ref<Expr> VarAndOnes = AndExpr::create(Var, Ones);
        OneBitsMap[Origin] = EqExpr::create(VarAndOnes, Ones);
      }
      if (Origin->NonZero)
        NonZeroBitsMap[Origin] = NeExpr::create(Var, klee::ConstantExpr::create(0, Width));
      if (Origin->NonNegative)
        NonNegBitsMap[Origin] = SleExpr::create(klee::ConstantExpr::create(0, Width), Var);
      if (Origin->PowOfTwo) {
        ref<Expr> Zero = klee::ConstantExpr::create(0, Width);
        PowerTwoBitsMap[Origin] = AndExpr::create(NeExpr::create(Var, Zero),
                                                  EqExpr::create(AndExpr::create(Var,
                                                  SubExpr::create(Var, klee::ConstantExpr::create(1, Width))),
                                                  Zero));
      }
      if (Origin->Negative)
        NegBitsMap[Origin] = SltExpr::create(Var, klee::ConstantExpr::create(0, Width));
      if (Origin->NumSignBits > 1) {
        ref<Expr> Res = AShrExpr::create(Var, klee::ConstantExpr::create(Width - Origin->NumSignBits, Width));
        ref<Expr> TestOnes = AShrExpr::create(ShlExpr::create(klee::ConstantExpr::create(1, Width),
                                                              klee::ConstantExpr::create(Width - 1, Width)),
                                              klee::ConstantExpr::create(Width - 1, Width));
        SignBitsMap[Origin] = OrExpr::create(EqExpr::create(Res, TestOnes),
                                             EqExpr::create(Res, klee::ConstantExpr::create(0, Width)));
      }
    }
    return Var;
  }

};

}

std::unique_ptr<ExprBuilder> souper::createKLEEBuilder(InstContext &IC) {
  return std::unique_ptr<ExprBuilder>(new KLEEBuilder(IC));
}
