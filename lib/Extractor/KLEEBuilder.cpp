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
#include "souper/Extractor/ExprBuilder.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/LoopInfo.h"

using namespace klee;
using namespace souper;

namespace {

static llvm::cl::opt<bool> DumpKLEEExprs(
    "dump-klee-exprs",
    llvm::cl::desc("Dump KLEE expressions after SMTLIB queries"),
    llvm::cl::init(false));

class KLEEBuilder : public ExprBuilder {
  UniqueNameSet ArrayNames;
  std::vector<std::unique_ptr<Array>> Arrays;
  std::map<Inst *, ref<Expr>> ExprMap;
  std::vector<Inst *> Vars;

public:
  KLEEBuilder(InstContext &IC) : ExprBuilder(IC) {}

  std::string GetExprStr(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars, bool Negate) override {
    Inst *Cand = GetCandidateExprForReplacement(BPCs, PCs, Mapping, /*Precondition=*/0, Negate);
    if (!Cand)
      return std::string();
    prepopulateExprMap(Cand);
    ref<Expr> E = get(Cand);

    std::string SStr;
    llvm::raw_string_ostream SS(SStr);
    std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SS));
    PP->setForceNoLineBreaks(true);
    PP->scan(E);
    PP->print(E);

    return SS.str();
  }

  std::string BuildQuery(const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         InstMapping Mapping,
                         std::vector<Inst *> *ModelVars,
                         Inst *Precondition, bool Negate) override {
    std::string SMTStr;
    llvm::raw_string_ostream SMTSS(SMTStr);
    ConstraintManager Manager;
    Inst *Cand = GetCandidateExprForReplacement(BPCs, PCs, Mapping, Precondition, Negate);
    if (!Cand)
      return std::string();
    prepopulateExprMap(Cand);
    ref<Expr> E = get(Cand);
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
    case Inst::Hole:
    case Inst::Var:
      return makeSizedArrayRead(I->Width, I->Name, I);
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
      constexpr unsigned bytelen = 8;
      ref<Expr> res = ExtractExpr::create(L, 0, bytelen);
      for (unsigned i = 1; i < L->getWidth() / bytelen; i++) {
	res = ConcatExpr::create(res, ExtractExpr::create(L, i * bytelen, bytelen));
      }

      return res;
    }
    case Inst::BitReverse: {
      ref<Expr> L = get(Ops[0]);
      auto res = ExtractExpr::create(L, 0, 1);
      for (unsigned i = 1; i < L->getWidth(); i++) {
	auto tmp = ExtractExpr::create(L, i, 1);
	res = ConcatExpr::create(res, tmp);
      }
      return res;
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
    case Inst::FShl:
    case Inst::FShr: {
      unsigned IWidth = I->Width;
      ref<Expr> High = get(Ops[0]);
      ref<Expr> Low = get(Ops[1]);
      ref<Expr> ShAmt = get(Ops[2]);
      ref<Expr> ShAmtModWidth =
          URemExpr::create(ShAmt, klee::ConstantExpr::create(IWidth, IWidth));
      ref<Expr> Concatenated = ConcatExpr::create(High, Low);
      unsigned CWidth = Concatenated->getWidth();
      ref<Expr> ShAmtModWidthZExt = ZExtExpr::create(ShAmtModWidth, CWidth);
      ref<Expr> Shifted =
          I->K == Inst::FShl
              ? ShlExpr::create(Concatenated, ShAmtModWidthZExt)
              : LShrExpr::create(Concatenated, ShAmtModWidthZExt);
      unsigned BitOffset = I->K == Inst::FShr ? 0 : IWidth;
      return ExtractExpr::create(Shifted, BitOffset, IWidth);
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
    case Inst::SAddSat: {
      ref<Expr> add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      auto sextL = SExtExpr::create(get(Ops[0]), I->Width + 1);
      auto sextR = SExtExpr::create(get(Ops[1]), I->Width + 1);
      auto addExt = AddExpr::create(sextL, sextR);
      auto smin = klee::ConstantExpr::alloc(llvm::APInt::getSignedMinValue(I->Width));
      auto smax = klee::ConstantExpr::alloc(llvm::APInt::getSignedMaxValue(I->Width));
      auto sminExt = SExtExpr::create(smin, I->Width + 1);
      auto smaxExt = SExtExpr::create(smax, I->Width + 1);
      auto pred = SleExpr::create(addExt, sminExt);

      auto pred2 = SgeExpr::create(addExt, smaxExt);
      auto select2 = SelectExpr::create(pred2, smax, add);

      return SelectExpr::create(pred, smin, select2);
    }
    case Inst::UAddSat: {
      ref<Expr> add = AddExpr::create(get(Ops[0]), get(Ops[1]));
      return SelectExpr::create(get(addnuwUB(I)), add, klee::ConstantExpr::alloc(llvm::APInt::getMaxValue(I->Width)));
    }
    case Inst::SSubSat: {
      ref<Expr> sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      auto sextL = SExtExpr::create(get(Ops[0]), I->Width + 1);
      auto sextR = SExtExpr::create(get(Ops[1]), I->Width + 1);
      auto subExt = SubExpr::create(sextL, sextR);
      auto smin = klee::ConstantExpr::alloc(llvm::APInt::getSignedMinValue(I->Width));
      auto smax = klee::ConstantExpr::alloc(llvm::APInt::getSignedMaxValue(I->Width));
      auto sminExt = SExtExpr::create(smin, I->Width + 1);
      auto smaxExt = SExtExpr::create(smax, I->Width + 1);
      auto pred = SleExpr::create(subExt, sminExt);

      auto pred2 = SgeExpr::create(subExt, smaxExt);
      auto select2 = SelectExpr::create(pred2, smax, sub);

      return SelectExpr::create(pred, smin, select2);
    }
    case Inst::USubSat: {
      ref<Expr> sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
      return SelectExpr::create(get(subnuwUB(I)), sub, klee::ConstantExpr::alloc(llvm::APInt::getMinValue(I->Width)));
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

  // get() is recursive. It already has problems with running out of stack
  // space with very trivial inputs, since there are a lot of IR instructions
  // it knows how to produce, and thus a lot of possible replacements.
  // But it has built-in caching. And recursion only kicks in if the Inst is not
  // found in cache. Thus we simply need to *try* to prepopulate the cache.
  // Note that we can't really split `get()` into `getSimple()` and
  // `getOrBuildRecursive()` because we won't reach every single of these Inst
  // because we only look at Ops...
  void prepopulateExprMap(Inst *Root) {
    // Collect all Inst that are reachable from this root. Go Use->Def.
    // An assumption is being made that there are no circular references.
    // Note that we really do want a simple vector, and do want duplicate
    // elements. In other words, if we have already added that Inst into vector,
    // we "move" it to the back of the vector. But without actually moving.
    llvm::SmallVector<Inst *, 32> AllInst;
    AllInst.emplace_back(Root);
    // Visit every Inst in the vector, but address them by index,
    // since we will be appending new entries at the end.
    for (size_t InstNum = 0; InstNum < AllInst.size(); InstNum++) {
      Inst *CurrInst = AllInst[InstNum];
      const std::vector<Inst *> &Ops = CurrInst->orderedOps();
      AllInst.insert(AllInst.end(), Ops.rbegin(), Ops.rend());
    }

    // And now, 'get()' every Inst, going in Def->Use direction.
    // That is, when we visit Inst N2, that has Ops N0 and N1,
    // the Inst's for N0 and N1 were already generated.
    llvm::for_each(llvm::reverse(AllInst), [this](Inst *CurrInst) {
      switch (CurrInst->K) {
      case Inst::UntypedConst:
      case Inst::SAddWithOverflow:
      case Inst::UAddWithOverflow:
      case Inst::SSubWithOverflow:
      case Inst::USubWithOverflow:
      case Inst::SMulWithOverflow:
      case Inst::UMulWithOverflow:
        return; // Avoid pre-generation for some Inst'ructions.
      default:
        break;
      }
      (void)get(CurrInst);
    });
  }

  ref<Expr> makeSizedArrayRead(unsigned Width, llvm::StringRef Name, Inst *Origin) {
    std::string NameStr;
    if (Name.empty())
      NameStr = "arr";
    else if (Name[0] >= '0' && Name[0] <= '9')
      NameStr = ("a" + Name).str();
    else
      NameStr = Name;
    Arrays.emplace_back(
     new Array(ArrayNames.makeName(NameStr), 1, 0, 0, Expr::Int32, Width));
    Vars.push_back(Origin);

    UpdateList UL(Arrays.back().get(), 0);
    return ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
  }

};

}

std::unique_ptr<ExprBuilder> souper::createKLEEBuilder(InstContext &IC) {
  return std::unique_ptr<ExprBuilder>(new KLEEBuilder(IC));
}
