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
  ReplacementContext Context;
  UniqueNameSet VarNames;
  std::vector<Inst *> Vars;
  std::map<Inst *, expr> ExprMap;

public:
  Z3Builder(InstContext &IC) : ExprBuilder(IC) {}

  std::string GetExprStr(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars, bool Negate) override {
    Inst *Cand = GetCandidateExprForReplacement(BPCs, PCs, Mapping, Negate);
    if (!Cand)
      return std::string();
    expr E = get(Cand);
    E = E.simplify();
    solver s(c);
    s.add(E == c.bv_val(0, E.get_sort().bv_size()));

    return s.to_smt2();
  }

  std::string BuildQuery(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars, bool Negate) override {
    Inst *Cand = GetCandidateExprForReplacement(BPCs, PCs, Mapping, Negate);
    if (!Cand)
      return std::string();
    expr E = get(Cand);
    //E = E.simplify();
    solver s(c);
    s.add(E == c.bv_val(0, E.get_sort().bv_size()));
    //
    std::string SMTStr;
    SMTStr += "(set-option :produce-models true)\n";
    SMTStr += "(set-logic QF_BV )\n";
    SMTStr += s.to_smt2();
    if (ModelVars) {
      for (auto Var : Vars) {
        SMTStr += "(get-value (" + Var->Name + ") )\n";
        ModelVars->push_back(Var);
      }
    }
    SMTStr += "(exit)\n";

    //llvm::outs() << SMTStr << "\n";

    return SMTStr;
  }

private:
  expr countOnes(expr L) {
     unsigned Width = L.get_sort().bv_size();
     expr Count = c.bv_val(0, Width);
     for (unsigned J=0; J<Width; J++) {
       expr Bit = L.extract(J, 0);
       expr BitExt = expr(c, Z3_mk_zero_ext(c, Width, Bit));
       Count = Count + BitExt;
     }
     return Count;
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
    //llvm::outs() << "## getting instruction: " << Inst::getKindName(I->K) << "\n";
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
    case Inst::Phi: {
      const auto &PredExpr = I->B->PredVars;
      assert((PredExpr.size() || Ops.size() == 1) && "there must be block predicates");
      expr E = get(Ops[0]);
      // e.g. P2 ? (P1 ? Op1_Expr : Op2_Expr) : Op3_Expr
      for (unsigned J = 1; J < Ops.size(); ++J) {
        E = ite(get(PredExpr[J-1]), E, get(Ops[J]));
      }
      return E;
    }
    case Inst::Add: {
      //return buildAssoc(AddExpr::create, Ops);
      expr E = get(Ops[0]);
      for (auto Op : Ops) {
        if (Op == Ops[0])
          continue;
        E = E + get(Op);
      }
      return E;
    }
    case Inst::AddNSW:
    case Inst::AddNUW:
    case Inst::AddNW: {
      //ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      return get(Ops[0]) + get(Ops[1]);
    }
    case Inst::Sub:
    case Inst::SubNSW:
    case Inst::SubNUW:
    case Inst::SubNW: {
      //return SubExpr::create(get(Ops[0]), get(Ops[1]));
      return get(Ops[0]) - get(Ops[1]);
    }
    case Inst::Mul: {
      //return buildAssoc(MulExpr::create, Ops);
      expr E = get(Ops[0]);
      for (auto Op : Ops) {
        if (Op == Ops[0])
          continue;
        E = E * get(Op);
      }
      return E;
    }
    case Inst::MulNSW:
    case Inst::MulNUW:
    case Inst::MulNW: {
      //ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
      return get(Ops[0]) * get(Ops[1]);
    }
    // TODO: Handle divide-by-zero explicitly
    case Inst::UDiv:
    case Inst::SDiv:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::URem:
    case Inst::SRem: { // Fall-through
      // If the second oprand is 0, then it definitely causes UB.
      // Just return a constant zero.
      if (Ops[1]->K == Inst::Const && Ops[1]->Val.isNullValue())
        return c.bv_val(0, I->Width);

      expr R = get(Ops[1]);
  
      switch (I->K) {
      default:
        break;
  
      case Inst::UDiv:
      case Inst::UDivExact: {
        //ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
        return expr(c, Z3_mk_bvudiv(c, get(Ops[0]), get(Ops[1])));
      }
      case Inst::SDiv:
      case Inst::SDivExact: {
        //ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
        return expr(c, Z3_mk_bvsdiv(c, get(Ops[0]), get(Ops[1])));
      }
      case Inst::URem: {
        //ref<Expr> Urem = URemExpr::create(get(Ops[0]), R);
        return expr(c, Z3_mk_bvurem(c, get(Ops[0]), get(Ops[1])));
      }
      case Inst::SRem: {
        //ref<Expr> Srem = SRemExpr::create(get(Ops[0]), R);
        return expr(c, Z3_mk_bvsrem(c, get(Ops[0]), get(Ops[1])));
      }
      llvm_unreachable("unknown kind");
    }
    }
  
    case Inst::And: {
      //return buildAssoc(AndExpr::create, Ops);
      expr E = get(Ops[0]);
      for (auto Op : Ops) {
        if (Op == Ops[0])
          continue;
        E = E & get(Op);
      }
      return E;
    }
    case Inst::Or: {
      //return buildAssoc(OrExpr::create, Ops);
      expr E = get(Ops[0]);
      for (auto Op : Ops) {
        if (Op == Ops[0])
          continue;
        E = E | get(Op);
      }
      return E;
    }
    case Inst::Xor: {
      //return buildAssoc(XorExpr::create, Ops);
      expr E = get(Ops[0]);
      for (auto Op : Ops) {
        if (Op == Ops[0])
          continue;
        E = E ^ get(Op);
      }
      return E;
    }
    case Inst::Shl:
    case Inst::ShlNSW:
    case Inst::ShlNUW:
    case Inst::ShlNW: {
      //ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvshl(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::LShr:
    case Inst::LShrExact: {
      //ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvlshr(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::AShr:
    case Inst::AShrExact: {
      //ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvashr(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::Select: {
      //return SelectExpr::create(get(Ops[0]), get(Ops[1]), get(Ops[2]));
      return ite(get(Ops[0]), get(Ops[1]), get(Ops[2]));
    }
    case Inst::ZExt: {
      //return ZExtExpr::create(get(Ops[0]), I->Width);
      return expr(c, Z3_mk_zero_ext(c, I->Width, get(Ops[0])));
    }
    case Inst::SExt:
      //return SExtExpr::create(get(Ops[0]), I->Width);
      return expr(c, Z3_mk_sign_ext(c, I->Width, get(Ops[0])));
    case Inst::Trunc: {
      //return ExtractExpr::create(get(Ops[0]), 0, I->Width);
      return get(Ops[0]).extract(I->Width-1, 0);
    }
    case Inst::Eq: {
      //return EqExpr::create(get(Ops[0]), get(Ops[1]));
      return ite(get(Ops[0]) == get(Ops[1]), c.bv_val(1, 1), c.bv_val(0, 1));
    }
    case Inst::Ne: {
      //return NeExpr::create(get(Ops[0]), get(Ops[1]));
      return ite(get(Ops[0]) != get(Ops[1]), c.bv_val(1, 1), c.bv_val(0, 1));
    }
    case Inst::Ult: {
      //return UltExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvult(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::Slt: {
      //return SltExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvslt(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::Ule: {
      //return UleExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvule(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::Sle: {
      //return SleExpr::create(get(Ops[0]), get(Ops[1]));
      return expr(c, Z3_mk_bvsle(c, get(Ops[0]), get(Ops[1])));
    }
    case Inst::CtPop:
      return countOnes(get(Ops[0]));
    case Inst::BSwap: {
      expr L = get(Ops[0]);
      unsigned Width = L.get_sort().bv_size();
      expr_vector EV(c);
      if (Width == 16) {
        //return ConcatExpr::create(ExtractExpr::create(L, 0, 8),
        //                          ExtractExpr::create(L, 8, 8));
        EV.push_back(L.extract(7, 0));
        EV.push_back(L.extract(15, 8));
      }
      else if (Width == 32) {
        //return ConcatExpr::create4(ExtractExpr::create(L, 0, 8),
        //                           ExtractExpr::create(L, 8, 8),
        //                           ExtractExpr::create(L, 16, 8),
        //                           ExtractExpr::create(L, 24, 8));
        EV.push_back(L.extract(7, 0));
        EV.push_back(L.extract(15, 8));
        EV.push_back(L.extract(23, 16));
        EV.push_back(L.extract(31, 24));
      }
      else if (Width == 64) {
        //return ConcatExpr::create8(ExtractExpr::create(L, 0, 8),
        //                           ExtractExpr::create(L, 8, 8),
        //                           ExtractExpr::create(L, 16, 8),
        //                           ExtractExpr::create(L, 24, 8),
        //                           ExtractExpr::create(L, 32, 8),
        //                           ExtractExpr::create(L, 40, 8),
        //                           ExtractExpr::create(L, 48, 8),
        //                           ExtractExpr::create(L, 56, 8));
        EV.push_back(L.extract(7, 0));
        EV.push_back(L.extract(15, 8));
        EV.push_back(L.extract(23, 16));
        EV.push_back(L.extract(31, 24));
        EV.push_back(L.extract(39, 32));
        EV.push_back(L.extract(47, 40));
        EV.push_back(L.extract(55, 48));
        EV.push_back(L.extract(63, 56));
      }
      return concat(EV);
    }
    case Inst::Cttz: {
      expr Val = get(Ops[0]);
      unsigned Width = Val.get_sort().bv_size();
      for (unsigned i=0, j=0; j<Width/2; i++) {
        j = 1<<i;
        //Val = OrExpr::create(Val, ShlExpr::create(Val,
        //                     klee::ConstantExpr::create(j, Width)));
        Val = Val | expr(c, Z3_mk_bvshl(c, Val, c.bv_val(j, Width)));
      }
      //return SubExpr::create(klee::ConstantExpr::create(Width, Width),
      //                       countOnes(Val));
      return c.bv_val(Width, Width) - countOnes(Val);
    }
    case Inst::Ctlz: {
      expr Val = get(Ops[0]);
      unsigned Width = Val.get_sort().bv_size();
      for (unsigned i=0, j=0; j<Width/2; i++) {
        j = 1<<i;
        //Val = OrExpr::create(Val, LShrExpr::create(Val,
        //                     klee::ConstantExpr::create(j, Width)));
        Val = Val | expr(c, Z3_mk_bvlshr(c, Val, c.bv_val(j, Width)));
      }
      //return SubExpr::create(klee::ConstantExpr::create(Width, Width),
      //                       countOnes(Val));
      return c.bv_val(Width, Width) - countOnes(Val);
    }
    case Inst::SAddO: {
      //return XorExpr::create(get(addnswUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
      return get(addnswUB(I)) ^ c.bv_val(1, 1);
    }
    case Inst::UAddO: {
      //return XorExpr::create(get(addnuwUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
      return get(addnuwUB(I)) ^ c.bv_val(1, 1);
    }
    case Inst::SSubO: {
      //return XorExpr::create(get(subnswUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
      return get(subnswUB(I)) ^ c.bv_val(1, 1);
    }
    case Inst::USubO: {
      //return XorExpr::create(get(subnuwUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
      return get(subnuwUB(I)) ^ c.bv_val(1, 1);
    }
    case Inst::SMulO: {
      //return XorExpr::create(get(mulnswUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
      return get(mulnswUB(I)) ^ c.bv_val(1, 1);
    }
    case Inst::UMulO: {
      //return XorExpr::create(get(mulnuwUB(I)), klee::ConstantExpr::create(1, Expr::Bool));
      return get(mulnuwUB(I)) ^ c.bv_val(1, 1);
    }
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
  
  expr get(Inst *I) {
    if (ExprMap.count(I)) 
      return ExprMap.at(I);
    expr E = build(I);
#if 0
    llvm::outs() << "@@@ sort kind for " << Inst::getKindName(I->K) << ": " << E.get_sort().sort_kind() << "\n";
    ReplacementContext Context;
    PrintReplacementRHS(llvm::outs(), I, Context);
    llvm::outs() << "@@@ sort name: " << E.get_sort().name() << "\n";
#endif
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
    NameStr = VarNames.makeName(NameStr);
    expr E = c.bv_const(NameStr.c_str(), Width);
    Origin->Name = NameStr;
    Vars.emplace_back(Origin);
  
    return E;
  }

};

}

std::unique_ptr<ExprBuilder> souper::createZ3Builder(InstContext &IC) {
  return std::unique_ptr<ExprBuilder>(new Z3Builder(IC));
}
