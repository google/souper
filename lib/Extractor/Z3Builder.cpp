// Copyright 2018 The Souper Authors. All rights reserved.
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

#include "llvm/Analysis/LoopInfo.h"
#include "souper/Extractor/ExprBuilder.h"

#include "z3++.h"

using namespace llvm;
using namespace souper;
using namespace z3;

namespace {

class Z3Builder : public ExprBuilder {
  context c;
  UniqueNameSet ConstNames;
  std::vector<expr> Consts;
  std::map<Inst *, expr> ExprMap;
  std::vector<Inst *> Vars;

public:
  Z3Builder(InstContext &IC) {
    LIC = &IC;
  }

  std::string GetExprStr(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars, bool Negate) override {
    Inst *Cand = GetCandidateExprForReplacement(BPCs, PCs, Mapping, Negate);
    if (!Cand)
      return std::string();
#if 0
    ref<Expr> E = get(Cand);

    std::string SStr;
    llvm::raw_string_ostream SS(SStr);
    std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SS));
    PP->setForceNoLineBreaks(true);
    PP->scan(E);
    PP->print(E);

    return SS.str();
#endif
    return std::string();
  }

  std::string BuildQuery(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars, bool Negate) override {
    std::string SMTStr;
    llvm::raw_string_ostream SMTSS(SMTStr);
    Inst *Cand = GetCandidateExprForReplacement(BPCs, PCs, Mapping, Negate);
    if (!Cand)
      return std::string();
#if 0
    ref<Expr> E = get(Cand);
    ConstraintManager Manager;
    Query KQuery(Manager, E);
    ExprSMTLIBPrinter Printer;
    Printer.setOutput(SMTSS);
    Printer.setQuery(KQuery);
    std::vector<const klee::Array *> Arr;
    if (ModelVars) {
      for (unsigned I = 0; I != Vars.size(); ++I) {
        if (Vars[I]) {
          Arr.push_back(Arrays[I].get());
          ModelVars->push_back(Vars[I]);
        }
      }
      Printer.setArrayValuesToGet(Arr);
    }
    Printer.generateOutput();
  
    return SMTSS.str();
#endif
    return std::string();
  }

private:
  expr countOnes(expr L) {
#if 0
     Expr::Width Width = L->getWidth();
     ref<Expr> Count =  klee::ConstantExpr::alloc(llvm::APInt(Width, 0));
     for (unsigned i=0; i<Width; i++) {
       ref<Expr> Bit = ExtractExpr::create(L, i, Expr::Bool);
       ref<Expr> BitExt = ZExtExpr::create(Bit, Width);
       Count = AddExpr::create(Count, BitExt);
     }
     return Count;
#endif
     return c.bool_val(true);
  }

  expr buildAssoc(
      std::function<expr(expr, expr)> F,
      llvm::ArrayRef<Inst *> Ops) {
    expr E = get(Ops[0]);
    for (Inst *I : llvm::ArrayRef<Inst *>(Ops.data()+1, Ops.size()-1)) {
      E = F(E, get(I));
    }
    return E;
  }

  expr build(Inst *I) {
    const std::vector<Inst *> &Ops = I->orderedOps();
    switch (I->K) {
    case Inst::UntypedConst:
      assert(0 && "unexpected kind");
    case Inst::Const: {
      //return klee::ConstantExpr::alloc(I->Val);
      if (I->Val.isNegative())
        return c.bv_val((int)(I->Val.getSExtValue()), I->Width);
      else
        return c.bv_val((unsigned)(I->Val.getZExtValue()), I->Width);
    }
    case Inst::Var:
      return makeSizedConst(I->Width, I->Name, I);
    // REMOVE AFTER HECiIRE
    default:
      break;
    }
    //
#if 0
    case Inst::Phi: {
      const auto &PredExpr = I->B->PredVars;
      assert((PredExpr.size() || Ops.size() == 1) && "there must be block predicates");
      ref<Expr> E = get(Ops[0]);
      // e.g. P2 ? (P1 ? Op1_Expr : Op2_Expr) : Op3_Expr
      for (unsigned J = 1; J < Ops.size(); ++J) {
        E = SelectExpr::create(get(PredExpr[J-1]), E, get(Ops[J]));
      }
      return E;
    }
    case Inst::Add:
      return buildAssoc(AddExpr::create, Ops);
    case Inst::AddNSW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      return Add;
    }
    case Inst::AddNUW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      return Add;
    }
    case Inst::AddNW: {
      ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      return Add;
    }
    case Inst::Sub:
      return SubExpr::create(get(Ops[0]), get(Ops[1]));
    case Inst::SubNSW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      return Sub;
    }
    case Inst::SubNUW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      return Sub;
    }
    case Inst::SubNW: {
      ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      return Sub;
    }
    case Inst::Mul:
      return buildAssoc(MulExpr::create, Ops);
    case Inst::MulNSW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      return Mul;
    }
    case Inst::MulNUW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      return Mul;
    }
    case Inst::MulNW: {
      ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      return Mul;
    }
  
    // We introduce these extra checks here because KLEE invokes llvm::APInt's
    // div functions, which crash upon divide-by-zero.
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
        return klee::ConstantExpr::create(0, Ops[1]->Width);
      }
  
      switch (I->K) {
      default:
        break;
  
      case Inst::UDiv: {
        ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        return Udiv;
      }
      case Inst::SDiv: {
        ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        return Sdiv;
      }
      case Inst::UDivExact: {
        ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        return Udiv;
      }
      case Inst::SDivExact: {
        ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        return Sdiv;
      }
      case Inst::URem: {
        ref<Expr> Urem = URemExpr::create(get(Ops[0]), R);
        return Urem;
      }
      case Inst::SRem: {
        ref<Expr> Srem = SRemExpr::create(get(Ops[0]), R);
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
      return Result;
    }
    case Inst::ShlNSW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      return Result;
    }
    case Inst::ShlNUW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      return Result;
    }
    case Inst::ShlNW: {
      ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      return Result;
    }
    case Inst::LShr: {
      ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      return Result;
    }
    case Inst::LShrExact: {
      ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      return Result;
    }
    case Inst::AShr: {
      ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
      return Result;
    }
    case Inst::AShrExact: {
      ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
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
#endif
    return c.bool_val(true);
  }
  
  expr get(Inst *I) {
    if (ExprMap.count(I)) 
      return ExprMap.at(I);
    expr E = build(I);
    assert(E.get_sort().bv_size() == I->Width);
    ExprMap.insert(std::make_pair(I, E));
    return E;
  }
  
  expr makeSizedConst(unsigned Width, StringRef Name, Inst *Origin) {
    std::string NameStr;
    if (Name.empty())
      NameStr = "arr";
    else if (Name[0] >= '0' && Name[0] <= '9')
      NameStr = ("a" + Name).str();
    else
      NameStr = Name;
    expr E = c.bv_const(ConstNames.makeName(NameStr).c_str(), Width);
    Consts.emplace_back(E);
    Vars.push_back(Origin);
  
    return E;
  }

};

}

std::unique_ptr<ExprBuilder> souper::createZ3Builder(InstContext &IC) {
  return std::unique_ptr<ExprBuilder>(new Z3Builder(IC));
}
