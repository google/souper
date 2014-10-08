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

#include "klee/Expr.h"
#include "klee/util/Ref.h"
#include "llvm/ADT/EquivalenceClasses.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/PassManager.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <map>
#include <memory>
#include <sstream>

using namespace llvm;
using namespace klee;
using namespace souper;

std::string InstOrigin::getFunctionName() const {
  if (Inst) {
    const Function *F = Inst->getParent()->getParent();
    if (F->hasLocalLinkage()) {
      return (F->getParent()->getModuleIdentifier() + ":" + F->getName()).str();
    } else {
      return F->getName();
    }
  }

  return FunctionName;
}

void CandidateReplacement::printFunction(llvm::raw_ostream &Out) const {
  Out << "; Function: " << Origin.getFunctionName() << '\n';
}

void CandidateReplacement::printLHS(llvm::raw_ostream &Out) const {
  PrintReplacementLHS(Out, PCs, Mapping.LHS);
}

void CandidateReplacement::print(llvm::raw_ostream &Out) const {
  PrintReplacement(Out, PCs, Mapping);
}

namespace {

struct ExprBuilder {
  ExprBuilder(const ExprBuilderOptions &Opts, Module *M, const LoopInfo *LI,
              InstContext &IC, ExprBuilderContext &EBC)
      : Opts(Opts), DL(M->getDataLayout()), LI(LI), IC(IC), EBC(EBC) {}

  const ExprBuilderOptions &Opts;
  const DataLayout *DL;
  const LoopInfo *LI;
  InstContext &IC;
  ExprBuilderContext &EBC;

  Inst *makeArrayRead(Value *V);
  Inst *buildConstant(Constant *c);
  Inst *buildGEP(Inst *Ptr, gep_type_iterator begin, gep_type_iterator end);
  Inst *build(Value *V);
  Inst *getPathCondition(BasicBlock *BB);
  void addPathConditions(std::vector<InstMapping> &PCs, BasicBlock *BB);
  Inst *get(Value *V);
};

}

Inst *ExprBuilder::makeArrayRead(Value *V) {
  StringRef Name;
  if (Opts.NamedArrays)
    Name = V->getName();
  unsigned Width = DL->getTypeSizeInBits(V->getType());
  return IC.createVar(Width, Name);
}

Inst *ExprBuilder::buildConstant(Constant *c) {
  if (auto ci = dyn_cast<ConstantInt>(c)) {
    return IC.getConst(ci->getValue());
  } else if (auto cf = dyn_cast<ConstantFP>(c)) {
    return IC.getConst(cf->getValueAPF().bitcastToAPInt());
  } else if (isa<ConstantPointerNull>(c) || isa<UndefValue>(c) ||
             isa<ConstantAggregateZero>(c)) {
    return IC.getConst(APInt(DL->getTypeSizeInBits(c->getType()), 0));
  } else {
    // Constant{Expr, Vector, DataSequential, Struct, Array}
    return makeArrayRead(c);
  }
}

Inst *ExprBuilder::buildGEP(Inst *Ptr, gep_type_iterator begin,
                            gep_type_iterator end) {
  unsigned PSize = DL->getPointerSizeInBits();
  for (auto i = begin; i != end; ++i) {
    if (StructType *ST = dyn_cast<StructType>(*i)) {
      const StructLayout *SL = DL->getStructLayout(ST);
      ConstantInt *CI = cast<ConstantInt>(i.getOperand());
      uint64_t Addend = SL->getElementOffset((unsigned) CI->getZExtValue());
      if (Addend != 0) {
        Ptr = IC.getInst(Inst::Add, PSize,
                         {Ptr, IC.getConst(APInt(PSize, Addend))});
      }
    } else {
      SequentialType *SET = cast<SequentialType>(*i);
      uint64_t ElementSize =
        DL->getTypeStoreSize(SET->getElementType());
      Value *Operand = i.getOperand();
      Inst *Index = get(Operand);
      if (PSize > Index->Width)
        Index = IC.getInst(Inst::SExt, PSize, {Index});
      Inst *Addend = IC.getInst(
          Inst::Mul, PSize, {Index, IC.getConst(APInt(PSize, ElementSize))});
      Ptr = IC.getInst(Inst::Add, PSize, {Ptr, Addend});
    }
  }
  return Ptr;
}

Inst *ExprBuilder::build(Value *V) {
  if (auto C = dyn_cast<Constant>(V)) {
    return buildConstant(C);
  } else if (auto ICI = dyn_cast<ICmpInst>(V)) {
    if (!isa<IntegerType>(ICI->getType()))
      return makeArrayRead(V); // could be a vector operation

    Inst *L = get(ICI->getOperand(0)), *R = get(ICI->getOperand(1));
    switch (ICI->getPredicate()) {
      case ICmpInst::ICMP_EQ:
        return IC.getInst(Inst::Eq, 1, {L, R});
      case ICmpInst::ICMP_NE:
        return IC.getInst(Inst::Ne, 1, {L, R});
      case ICmpInst::ICMP_UGT:
        return IC.getInst(Inst::Ult, 1, {R, L});
      case ICmpInst::ICMP_UGE:
        return IC.getInst(Inst::Ule, 1, {R, L});
      case ICmpInst::ICMP_ULT:
        return IC.getInst(Inst::Ult, 1, {L, R});
      case ICmpInst::ICMP_ULE:
        return IC.getInst(Inst::Ule, 1, {L, R});
      case ICmpInst::ICMP_SGT:
        return IC.getInst(Inst::Slt, 1, {R, L});
      case ICmpInst::ICMP_SGE:
        return IC.getInst(Inst::Sle, 1, {R, L});
      case ICmpInst::ICMP_SLT:
        return IC.getInst(Inst::Slt, 1, {L, R});
      case ICmpInst::ICMP_SLE:
        return IC.getInst(Inst::Sle, 1, {L, R});
      default:
        llvm_unreachable("not ICmp");
    }
  } else if (auto BO = dyn_cast<BinaryOperator>(V)) {
    if (!isa<IntegerType>(BO->getType()))
      return makeArrayRead(V); // could be a vector operation

    Inst *L = get(BO->getOperand(0)), *R = get(BO->getOperand(1));
    Inst::Kind K;
    switch (BO->getOpcode()) {
      case Instruction::Add:
        if (BO->hasNoSignedWrap() && BO->hasNoUnsignedWrap())
          K = Inst::AddNW;
        else if (BO->hasNoSignedWrap())
          K = Inst::AddNSW;
        else if (BO->hasNoUnsignedWrap())
          K = Inst::AddNUW;
        else
          K = Inst::Add;
        break;
      case Instruction::Sub:
        if (BO->hasNoSignedWrap() && BO->hasNoUnsignedWrap())
          K = Inst::SubNW;
        else if (BO->hasNoSignedWrap())
          K = Inst::SubNSW;
        else if (BO->hasNoUnsignedWrap())
          K = Inst::SubNUW;
        else
          K = Inst::Sub;
        break;
      case Instruction::Mul:
        if (BO->hasNoSignedWrap() && BO->hasNoUnsignedWrap())
          K = Inst::MulNW;
        else if (BO->hasNoSignedWrap())
          K = Inst::MulNSW;
        else if (BO->hasNoUnsignedWrap())
          K = Inst::MulNUW;
        else
          K = Inst::Mul;
        break;
      case Instruction::UDiv:
        if (BO->isExact())
          K = Inst::UDivExact;
        else
          K = Inst::UDiv;
        break;
      case Instruction::SDiv:
        if (BO->isExact())
          K = Inst::SDivExact;
        else
          K = Inst::SDiv;
        break;
      case Instruction::URem:
        K = Inst::URem;
        break;
      case Instruction::SRem:
        K = Inst::SRem;
        break;
      case Instruction::And:
        K = Inst::And;
        break;
      case Instruction::Or:
        K = Inst::Or;
        break;
      case Instruction::Xor:
        K = Inst::Xor;
        break;
      case Instruction::Shl:
        if (BO->hasNoSignedWrap() && BO->hasNoUnsignedWrap())
          K = Inst::ShlNW;
        else if (BO->hasNoSignedWrap())
          K = Inst::ShlNSW;
        else if (BO->hasNoUnsignedWrap())
          K = Inst::ShlNUW;
        else
          K = Inst::Shl;
        break;
      case Instruction::LShr:
        if (BO->isExact())
          K = Inst::LShrExact;
        else
          K = Inst::LShr;
        break;
      case Instruction::AShr:
        if (BO->isExact())
          K = Inst::AShrExact;
        else
          K = Inst::AShr;
        break;
      default:
        llvm_unreachable("not BinOp");
    }
    return IC.getInst(K, L->Width, {L, R});
  } else if (auto Sel = dyn_cast<SelectInst>(V)) {
    if (!isa<IntegerType>(Sel->getType()))
      return makeArrayRead(V); // could be a vector operation
    Inst *C = get(Sel->getCondition()), *T = get(Sel->getTrueValue()),
         *F = get(Sel->getFalseValue());
    return IC.getInst(Inst::Select, T->Width, {C, T, F});
  } else if (auto Cast = dyn_cast<CastInst>(V)) {
    Inst *Op = get(Cast->getOperand(0));
    unsigned DestSize = DL->getTypeSizeInBits(Cast->getType());

    switch (Cast->getOpcode()) {
    case Instruction::BitCast:
      return Op;

    case Instruction::IntToPtr:
    case Instruction::PtrToInt:
      if (Op->Width > DestSize)
        return IC.getInst(Inst::Trunc, DestSize, {Op});
      else if (Op->Width < DestSize)
        return IC.getInst(Inst::ZExt, DestSize, {Op});
      else
        return Op;

    case Instruction::ZExt:
      if (!isa<IntegerType>(Cast->getType()))
        break; // could be a vector operation
      return IC.getInst(Inst::ZExt, DestSize, {Op});

    case Instruction::SExt:
      if (!isa<IntegerType>(Cast->getType()))
        break; // could be a vector operation
      return IC.getInst(Inst::SExt, DestSize, {Op});

    case Instruction::Trunc:
      if (!isa<IntegerType>(Cast->getType()))
        break; // could be a vector operation
      return IC.getInst(Inst::Trunc, DestSize, {Op});

    default:
      ; // fallthrough to return below
    }
  } else if (auto GEP = dyn_cast<GetElementPtrInst>(V)) {
    if (isa<VectorType>(GEP->getType()))
      return makeArrayRead(V); // vector operation
    return buildGEP(get(GEP->getOperand(0)), gep_type_begin(GEP),
                    gep_type_end(GEP));
  } else if (auto Phi = dyn_cast<PHINode>(V)) {
    // We can't look through phi nodes in loop headers because we might
    // encounter a previous iteration of an instruction and get a wrong result.
    // TODO: In principle we could track loop iterations and maybe even maintain
    // a separate set of values for each iteration (as in bounded model
    // checking).
    BasicBlock *BB = Phi->getParent();
    if (!LI->isLoopHeader(BB)) {
      BlockInfo &BI = EBC.BlockMap[BB];
      if (!BI.B) {
        std::copy(Phi->block_begin(), Phi->block_end(), std::back_inserter(BI.Preds));
        BI.B = IC.createBlock(BI.Preds.size());
      }
      std::vector<Inst *> Incomings;
      for (auto Pred : BI.Preds) {
        Incomings.push_back(get(Phi->getIncomingValueForBlock(Pred)));
      }
      return IC.getPhi(BI.B, Incomings);
    }
  }

  return makeArrayRead(V);
}

Inst *ExprBuilder::get(Value *V) {
  Inst *&E = EBC.InstMap[V];
  if (!E) {
    E = build(V);
  }
  return E;
}

void emplace_back_dedup(std::vector<InstMapping> &PCs, Inst *LHS, Inst *RHS) {
  for (auto &i : PCs)
    if (i.LHS == LHS && i.RHS == RHS)
      return;
  PCs.emplace_back(LHS, RHS);
}

void ExprBuilder::addPathConditions(std::vector<InstMapping> &PCs,
                                    BasicBlock *BB) {
  if (auto Pred = BB->getSinglePredecessor()) {
    addPathConditions(PCs, Pred);
    if (auto Branch = dyn_cast<BranchInst>(Pred->getTerminator())) {
      if (Branch->isConditional()) {
        emplace_back_dedup(
            PCs, get(Branch->getCondition()),
            IC.getConst(APInt(1, Branch->getSuccessor(0) == BB)));
      }
    } else if (auto Switch = dyn_cast<SwitchInst>(Pred->getTerminator())) {
      Inst *Cond = get(Switch->getCondition());
      ConstantInt *Case = Switch->findCaseDest(BB);
      if (Case) {
        emplace_back_dedup(PCs, Cond, get(Case));
      } else {
        // default
        std::vector<Inst *> Cases;
        for (auto I = Switch->case_begin(), E = Switch->case_end(); I != E;
             ++I) {
          Cases.push_back(
              IC.getInst(Inst::Ne, 1, {Cond, get(I.getCaseValue())}));
        }
        emplace_back_dedup(PCs, IC.getInst(Inst::And, 1, Cases),
                           IC.getConst(APInt(1, true)));
      }
    }
  }
}

namespace {

typedef llvm::EquivalenceClasses<Inst *> InstClasses;

// Add the variable set of I as an equivalence class to Vars, and return a
// reference to the leader of that equivalence class.
InstClasses::member_iterator AddVarSet(InstClasses::member_iterator Leader,
                                       InstClasses &Vars,
                                       llvm::DenseSet<Inst *> &SeenInsts,
                                       Inst *I) {
  if (I->K == Inst::Var) {
    if (Leader != Vars.member_end()) {
      return Vars.unionSets(Leader, Vars.findLeader(Vars.insert(I)));
    } else {
      return Vars.findLeader(Vars.insert(I));
    }
  } else {
    if (!SeenInsts.insert(I).second)
      return Leader;
    for (auto Op : I->Ops) {
      Leader = AddVarSet(Leader, Vars, SeenInsts, Op);
    }
    return Leader;
  }
}

// Add the variable sets of PCs as equivalence classes to Vars. Return a vector
// PCSets of the same size as PCs such that PCSets[i] is an arbitrary member of
// the equivalence class for PCs[i] (not necessarily its leader).
std::vector<Inst *> AddPCSets(const std::vector<InstMapping> &PCs,
                              InstClasses &Vars) {
  std::vector<Inst *> PCSets(PCs.size());
  for (unsigned i = 0; i != PCs.size(); ++i) {
    llvm::DenseSet<Inst *> SeenInsts;
    auto PCLeader =
        AddVarSet(Vars.member_end(), Vars, SeenInsts, PCs[i].LHS);
    if (PCLeader != Vars.member_end())
      PCSets[i] = *PCLeader;
  }
  return PCSets;
}

// Return a vector of relevant PCs for a candidate, namely those whose variable
// sets are in the same equivalence class as the candidate's.
std::vector<InstMapping> GetRelevantPCs(const std::vector<InstMapping> &PCs,
                                        const std::vector<Inst *> &PCSets,
                                        InstClasses Vars,
                                        InstMapping Cand) {
  llvm::DenseSet<Inst *> SeenInsts;
  auto Leader = AddVarSet(Vars.member_end(), Vars, SeenInsts, Cand.LHS);

  std::vector<InstMapping> RelevantPCs;
  for (unsigned i = 0; i != PCs.size(); ++i) {
    if (PCSets[i] != 0 && Vars.findLeader(PCSets[i]) == Leader)
      RelevantPCs.emplace_back(PCs[i]);
  }
  return RelevantPCs;
}

void ExtractExprCandidates(Function &F, const LoopInfo *LI,
                           const ExprBuilderOptions &Opts, InstContext &IC,
                           ExprBuilderContext &EBC,
                           FunctionCandidateSet &Result) {
  ExprBuilder EB(Opts, F.getParent(), LI, IC, EBC);

  for (auto &BB : F) {
    std::unique_ptr<BlockCandidateSet> BCS(new BlockCandidateSet);
    for (auto &I : BB) {
      if (I.getType()->isIntegerTy(1))
        BCS->Replacements.emplace_back(&I, InstMapping(EB.get(&I), 0));
    }
    if (!BCS->Replacements.empty()) {
      EB.addPathConditions(BCS->PCs, &BB);

      InstClasses Vars;
      auto PCSets = AddPCSets(BCS->PCs, Vars);

      for (auto &R : BCS->Replacements) {
        R.PCs = GetRelevantPCs(BCS->PCs, PCSets, Vars, R.Mapping);
      }

      Result.Blocks.emplace_back(std::move(BCS));
    }
  }
}

class ExtractExprCandidatesPass : public FunctionPass {
  static char ID;
  const ExprBuilderOptions &Opts;
  InstContext &IC;
  ExprBuilderContext &EBC;
  FunctionCandidateSet &Result;

public:
 ExtractExprCandidatesPass(const ExprBuilderOptions &Opts, InstContext &IC,
                           ExprBuilderContext &EBC,
                           FunctionCandidateSet &Result)
     : FunctionPass(ID), Opts(Opts), IC(IC), EBC(EBC), Result(Result) {}

  void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<LoopInfo>();
    Info.setPreservesAll();
  }

  bool runOnFunction(Function &F) {
    LoopInfo *LI = &getAnalysis<LoopInfo>();
    ExtractExprCandidates(F, LI, Opts, IC, EBC, Result);
    return false;
  }
};

char ExtractExprCandidatesPass::ID = 0;

}

FunctionCandidateSet souper::ExtractCandidatesFromPass(
    Function *F, const LoopInfo *LI, InstContext &IC, ExprBuilderContext &EBC,
    const ExprBuilderOptions &Opts) {
  FunctionCandidateSet Result;
  ExtractExprCandidates(*F, LI, Opts, IC, EBC, Result);
  return Result;
}

FunctionCandidateSet souper::ExtractCandidates(Function *F, InstContext &IC,
                                               ExprBuilderContext &EBC,
                                               const ExprBuilderOptions &Opts) {
  FunctionCandidateSet Result;

  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeAnalysis(Registry);

  FunctionPassManager FPM(F->getParent());
  FPM.add(new ExtractExprCandidatesPass(Opts, IC, EBC, Result));
  FPM.run(*F);

  return Result;
}
