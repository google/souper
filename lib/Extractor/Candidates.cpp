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

#include "souper/Extractor/Candidates.h"

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
#include "klee/Expr.h"
#include "klee/util/Ref.h"
#include "souper/Util/UniqueNameSet.h"

using namespace llvm;
using namespace klee;
using namespace souper;

namespace {

struct ExprBuilder {
  ExprBuilder(const ExprBuilderOptions &Opts, Module *M, const LoopInfo *LI,
              std::vector<std::unique_ptr<Array>> &Arrays)
      : Opts(Opts),
        DL(M->getDataLayout()),
        LI(LI),
        Arrays(Arrays),
        InstCondition(klee::ConstantExpr::create(1, Expr::Bool)) {}

  const ExprBuilderOptions &Opts;
  const DataLayout *DL;
  const LoopInfo *LI;
  std::map<const Value *, ref<Expr>> ExprMap;
  std::vector<std::unique_ptr<Array>> &Arrays;
  UniqueNameSet ArrayNames;
  ref<Expr> InstCondition;

  ref<Expr> makeSizedArrayRead(unsigned Width, StringRef Name = "arr");
  ref<Expr> makeArrayRead(Value *V);
  ref<Expr> buildConstant(Constant *c);
  ref<Expr> buildGEP(ref<Expr> Ptr, gep_type_iterator begin,
                     gep_type_iterator end);
  ref<Expr> build(Value *V);
  void addInstCondition(ref<Expr> E);
  ref<Expr> getPathCondition(BasicBlock *BB);
  ref<Expr> get(Value *V);
};

}

ref<Expr> ExprBuilder::makeSizedArrayRead(unsigned Width, StringRef Name) {
  Array *A = new Array(ArrayNames.makeName(Name), 1, 0, 0, Expr::Int32, Width);
  Arrays.emplace_back(A);

  UpdateList UL(A, 0);
  return ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
}

ref<Expr> ExprBuilder::makeArrayRead(Value *V) {
  StringRef Name = V->getName();
  if (Name.empty() || !Opts.NamedArrays)
    Name = "arr";
  unsigned Width = DL->getTypeSizeInBits(V->getType());
  return makeSizedArrayRead(Width, Name);
}

void ExprBuilder::addInstCondition(ref<Expr> E) {
  InstCondition = AndExpr::create(InstCondition, E);
}

ref<Expr> ExprBuilder::buildConstant(Constant *c) {
  if (auto ci = dyn_cast<ConstantInt>(c)) {
    return klee::ConstantExpr::alloc(ci->getValue());
  } else if (auto cf = dyn_cast<ConstantFP>(c)) {
    return klee::ConstantExpr::alloc(cf->getValueAPF().bitcastToAPInt());
  } else if (auto gv = dyn_cast<GlobalValue>(c)) {
    return makeArrayRead(gv);
  } else if (isa<ConstantPointerNull>(c) || isa<UndefValue>(c) ||
             isa<ConstantAggregateZero>(c)) {
    return klee::ConstantExpr::create(0, DL->getTypeSizeInBits(c->getType()));
  } else if (auto cds = dyn_cast<ConstantDataSequential>(c)) {
    std::vector<ref<Expr>> kids;
    for (unsigned i = 0, e = cds->getNumElements(); i != e; ++i) {
      ref<Expr> kid = get(cds->getElementAsConstant(i));
      kids.push_back(kid);
    }
    ref<Expr> res = ConcatExpr::createN(kids.size(), kids.data());
    return cast<klee::ConstantExpr>(res);
  } else if (auto cs = dyn_cast<ConstantStruct>(c)) {
    const StructLayout *sl = DL->getStructLayout(cs->getType());
    SmallVector<ref<Expr>, 4> kids;
    for (unsigned i = cs->getNumOperands(); i != 0; --i) {
      unsigned op = i-1;
      ref<Expr> kid = get(cs->getOperand(op));

      uint64_t thisOffset = sl->getElementOffsetInBits(op),
               nextOffset = (op == cs->getNumOperands() - 1)
                            ? sl->getSizeInBits()
                            : sl->getElementOffsetInBits(op+1);
      if (nextOffset-thisOffset > kid->getWidth()) {
        uint64_t paddingWidth = nextOffset-thisOffset-kid->getWidth();
        kids.push_back(klee::ConstantExpr::create(0, paddingWidth));
      }

      kids.push_back(kid);
    }
    return ConcatExpr::createN(kids.size(), kids.data());
  } else if (auto ca = dyn_cast<ConstantArray>(c)) {
    SmallVector<ref<Expr>, 4> kids;
    for (unsigned i = ca->getNumOperands(); i != 0; --i) {
      unsigned op = i-1;
      ref<Expr> kid = get(ca->getOperand(op));
      kids.push_back(kid);
    }
    return ConcatExpr::createN(kids.size(), kids.data());
  } else {
    // Constant{Expr, Vector}
    return makeArrayRead(c);
  }
}

ref<Expr> ExprBuilder::buildGEP(ref<Expr> Ptr, gep_type_iterator begin,
                                gep_type_iterator end) {
  for (auto i = begin; i != end; ++i) {
    if (StructType *ST = dyn_cast<StructType>(*i)) {
      const StructLayout *SL = DL->getStructLayout(ST);
      ConstantInt *CI = cast<ConstantInt>(i.getOperand());
      uint64_t Addend = SL->getElementOffset((unsigned) CI->getZExtValue());
      Ptr = AddExpr::create(
          Ptr, klee::ConstantExpr::create(Addend, DL->getPointerSizeInBits()));
    } else {
      SequentialType *SET = cast<SequentialType>(*i);
      uint64_t ElementSize =
        DL->getTypeStoreSize(SET->getElementType());
      Value *Operand = i.getOperand();
      ref<Expr> Index = get(Operand);
      Index = SExtExpr::create(Index, DL->getPointerSizeInBits());
      ref<Expr> Addend = MulExpr::create(
          Index,
          klee::ConstantExpr::create(ElementSize, DL->getPointerSizeInBits()));
      Ptr = AddExpr::create(Ptr, Addend);
    }
  }
  return Ptr;
}

ref<Expr> ExprBuilder::build(Value *V) {
  if (auto C = dyn_cast<Constant>(V)) {
    return buildConstant(C);
  } else if (auto IC = dyn_cast<ICmpInst>(V)) {
    if (!isa<IntegerType>(IC->getType()))
      return makeArrayRead(V); // could be a vector operation

    ref<Expr> L = get(IC->getOperand(0)), R = get(IC->getOperand(1));
    switch (IC->getPredicate()) {
      case ICmpInst::ICMP_EQ:
        return EqExpr::create(L, R);
      case ICmpInst::ICMP_NE:
        return NeExpr::create(L, R);
      case ICmpInst::ICMP_UGT:
        return UgtExpr::create(L, R);
      case ICmpInst::ICMP_UGE:
        return UgeExpr::create(L, R);
      case ICmpInst::ICMP_ULT:
        return UltExpr::create(L, R);
      case ICmpInst::ICMP_ULE:
        return UleExpr::create(L, R);
      case ICmpInst::ICMP_SGT:
        return SgtExpr::create(L, R);
      case ICmpInst::ICMP_SGE:
        return SgeExpr::create(L, R);
      case ICmpInst::ICMP_SLT:
        return SltExpr::create(L, R);
      case ICmpInst::ICMP_SLE:
        return SleExpr::create(L, R);
      default:
        llvm_unreachable("not ICmp");
    }
  } else if (auto BO = dyn_cast<BinaryOperator>(V)) {
    if (!isa<IntegerType>(BO->getType()))
      return makeArrayRead(V); // could be a vector operation

    ref<Expr> L = get(BO->getOperand(0)), R = get(BO->getOperand(1));
    switch (BO->getOpcode()) {
      case Instruction::Add: {
        ref<Expr> Add = AddExpr::create(L, R);
        if (BO->hasNoSignedWrap()) {
          unsigned Width = L->getWidth();
          ref<Expr> LMSB = ExtractExpr::create(L, Width-1, Expr::Bool);
          ref<Expr> RMSB = ExtractExpr::create(R, Width-1, Expr::Bool);
          ref<Expr> AddMSB = ExtractExpr::create(Add, Width-1, Expr::Bool);
          addInstCondition(Expr::createImplies(EqExpr::create(LMSB, RMSB),
                                               EqExpr::create(LMSB, AddMSB)));
        }
        return Add;
      }
      case Instruction::Sub: {
        ref<Expr> Sub = SubExpr::create(L, R);
        if (BO->hasNoSignedWrap()) {
          unsigned Width = L->getWidth();
          ref<Expr> LMSB = ExtractExpr::create(L, Width-1, Expr::Bool);
          ref<Expr> RMSB = ExtractExpr::create(R, Width-1, Expr::Bool);
          ref<Expr> SubMSB = ExtractExpr::create(Sub, Width-1, Expr::Bool);
          addInstCondition(Expr::createImplies(NeExpr::create(LMSB, RMSB),
                                               EqExpr::create(LMSB, SubMSB)));
        }
        return Sub;
      }
      case Instruction::Mul:
        return MulExpr::create(L, R);
      case Instruction::UDiv:
        return UDivExpr::create(L, R);
      case Instruction::SDiv:
        return SDivExpr::create(L, R);
      case Instruction::URem:
        return URemExpr::create(L, R);
      case Instruction::SRem:
        return SRemExpr::create(L, R);
      case Instruction::And:
        return AndExpr::create(L, R);
      case Instruction::Or:
        return OrExpr::create(L, R);
      case Instruction::Xor:
        return XorExpr::create(L, R);
      case Instruction::Shl:
        return ShlExpr::create(L, R);
      case Instruction::LShr:
        return LShrExpr::create(L, R);
      case Instruction::AShr:
        return AShrExpr::create(L, R);
      default:
        llvm_unreachable("not BinOp");
    }
  } else if (auto Sel = dyn_cast<SelectInst>(V)) {
    if (!isa<IntegerType>(Sel->getType()))
      return makeArrayRead(V); // could be a vector operation
    return SelectExpr::create(get(Sel->getCondition()),
                              get(Sel->getTrueValue()),
                              get(Sel->getFalseValue()));
  } else if (auto Cast = dyn_cast<CastInst>(V)) {
    ref<Expr> Op = get(Cast->getOperand(0));
    unsigned DestSize = DL->getTypeSizeInBits(Cast->getType());

    switch (Cast->getOpcode()) {
    case Instruction::BitCast:
      return Op;

    case Instruction::IntToPtr:
    case Instruction::PtrToInt:
    case Instruction::ZExt:
      if (!isa<IntegerType>(Cast->getType()))
        break; // could be a vector operation
      return ZExtExpr::create(Op, DestSize);

    case Instruction::SExt:
      if (!isa<IntegerType>(Cast->getType()))
        break; // could be a vector operation
      return SExtExpr::create(Op, DestSize);

    case Instruction::Trunc:
      if (!isa<IntegerType>(Cast->getType()))
        break; // could be a vector operation
      return ExtractExpr::create(Op, 0, DestSize);

    default:
      ; // fallthrough to return below
    }
  } else if (auto GEP = dyn_cast<GetElementPtrInst>(V)) {
    return buildGEP(get(GEP->getOperand(0)), gep_type_begin(GEP),
                    gep_type_end(GEP));
  } else if (auto Phi = dyn_cast<PHINode>(V)) {
    // We can't look through phi nodes in loop headers because we might
    // encounter a previous iteration of an instruction and get a wrong result.
    // TODO: In principle we could track loop iterations and maybe even maintain
    // a separate set of values for each iteration (as in bounded model
    // checking).
    if (!LI->isLoopHeader(Phi->getParent())) {
      ref<Expr> Val = get(Phi->getIncomingValue(0));
      for (unsigned i = 1, e = Phi->getNumIncomingValues(); i != e; ++i) {
        Val = SelectExpr::create(makeSizedArrayRead(Expr::Bool), Val,
                                 get(Phi->getIncomingValue(i)));
      }
      return Val;
    }
  }

  return makeArrayRead(V);
}

ref<Expr> ExprBuilder::get(Value *V) {
  ref<Expr> &E = ExprMap[V];
  if (E.isNull()) {
    E = build(V);
  }
  return E;
}

ref<Expr> ExprBuilder::getPathCondition(BasicBlock *BB) {
  if (auto Pred = BB->getSinglePredecessor()) {
    ref<Expr> PC = getPathCondition(Pred);
    if (auto Branch = dyn_cast<BranchInst>(Pred->getTerminator())) {
      if (Branch->isConditional()) {
        if (Branch->getSuccessor(0) == Pred) {
          return AndExpr::create(PC, get(Branch->getCondition()));
        } else {
          return AndExpr::create(
              PC, Expr::createIsZero(get(Branch->getCondition())));
        }
      }
    } else if (auto Switch = dyn_cast<SwitchInst>(Pred->getTerminator())) {
      ref<Expr> Cond = get(Switch->getCondition());
      ConstantInt *Case = Switch->findCaseDest(BB);
      if (Case) {
        return AndExpr::create(PC, EqExpr::create(Cond, get(Case)));
      } else {
        // default
        for (auto I = Switch->case_begin(), E = Switch->case_end(); I != E;
             ++I) {
          PC = AndExpr::create(PC, NeExpr::create(Cond, get(I.getCaseValue())));
        }
      }
    }

    return PC;
  }

  return klee::ConstantExpr::create(1, Expr::Bool);
}

namespace {

std::pair<ref<Expr>, ref<Expr>> GetExprPair(
    const ExprBuilderOptions &Opts, Module *M, LoopInfo *LI, Instruction *I,
    std::vector<std::unique_ptr<Array>> &Arrays) {
  ExprBuilder EB(Opts, M, LI, Arrays);
  ref<Expr> E = EB.get(I);
  ref<Expr> PC = EB.getPathCondition(I->getParent());
  ref<Expr> Cond = AndExpr::create(EB.InstCondition, PC);
  return std::make_pair(Expr::createImplies(Cond, E),
                        Expr::createImplies(Cond, Expr::createIsZero(E)));
}

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

bool IsTriviallySat(ref<Expr> E) {
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

class ExtractExprCandidatesPass : public FunctionPass {
  static char ID;
  const ExprBuilderOptions &Opts;
  std::vector<ExprCandidate> &Result;

public:
 ExtractExprCandidatesPass(const ExprBuilderOptions &Opts,
                           std::vector<ExprCandidate> &Result)
     : FunctionPass(ID), Opts(Opts), Result(Result) {}

  void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<LoopInfo>();
    Info.setPreservesAll();
  }

  bool runOnFunction(Function &F) {
    LoopInfo *LI = &getAnalysis<LoopInfo>();

    for (auto &BB : F) {
      for (auto &Inst : BB) {
        if (Inst.getType()->isIntegerTy(1)) {
          ExprCandidate Cand;
          Cand.Origin = &Inst;
          std::vector<std::unique_ptr<Array>> Arrays;
          auto E = GetExprPair(Opts, F.getParent(), LI, &Inst, Cand.Arrays);
          if (!IsTriviallySat(E.first))
            Cand.Queries.emplace_back(E.first, false, 1);
          if (!IsTriviallySat(E.second))
            Cand.Queries.emplace_back(E.second, true, 1);
          if (!Cand.Queries.empty())
            Result.emplace_back(std::move(Cand));
        }
      }
    }

    return false;
  }
};

char ExtractExprCandidatesPass::ID = 0;

}

std::vector<ExprCandidate> souper::ExtractExprCandidates(
    Module *M, const ExprBuilderOptions &Opts) {
  std::vector<ExprCandidate> Result;

  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeAnalysis(Registry);

  PassManager PM;
  PM.add(new ExtractExprCandidatesPass(Opts, Result));
  PM.run(*M);

  return Result;
}
