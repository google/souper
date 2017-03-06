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
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <map>
#include <memory>
#include <sstream>
#include <unordered_set>
#include <tuple>
#include "llvm/Analysis/ValueTracking.h"

static llvm::cl::opt<bool> ExploitBPCs(
    "souper-exploit-blockpcs",
    llvm::cl::desc("Exploit block path conditions (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> HarvestKnownBits(
    "souper-harvest-known-bits",
    llvm::cl::desc("Perform known bits analysis (default=true)"),
    llvm::cl::init(true));

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

void CandidateReplacement::printLHS(llvm::raw_ostream &Out,
                                    ReplacementContext &Context,
                                    bool printNames) const {
  PrintReplacementLHS(Out, BPCs, PCs, Mapping.LHS, Context, printNames);
}

void CandidateReplacement::print(llvm::raw_ostream &Out,
                                 bool printNames) const {
  PrintReplacement(Out, BPCs, PCs, Mapping, printNames);
}

namespace {

struct ExprBuilder {
  ExprBuilder(const ExprBuilderOptions &Opts, Module *M, const LoopInfo *LI,
              InstContext &IC, ExprBuilderContext &EBC)
      : Opts(Opts), DL(M->getDataLayout()), LI(LI), IC(IC), EBC(EBC) {}

  const ExprBuilderOptions &Opts;
  const DataLayout &DL;
  const LoopInfo *LI;
  InstContext &IC;
  ExprBuilderContext &EBC;

  void checkIrreducibleCFG(BasicBlock *BB,
                           BasicBlock *FirstBB,
                           std::unordered_set<const BasicBlock *> &VisitedBBs,
                           bool &Loop);
  bool isLoopEntryPoint(PHINode *Phi);
  Inst *makeArrayRead(Value *V);
  Inst *buildConstant(Constant *c);
  Inst *buildGEP(Inst *Ptr, gep_type_iterator begin, gep_type_iterator end);
  Inst *build(Value *V);
  void addPC(BasicBlock *BB, BasicBlock *Pred, std::vector<InstMapping> &PCs);
  void addPathConditions(BlockPCs &BPCs, std::vector<InstMapping> &PCs,
                         std::unordered_set<Block *> &VisitedBlocks,
                         BasicBlock *BB);
  Inst *get(Value *V);
};

}

// Use DFS to detect a possible loop, most likely an loop in an irreducible
// CFG. One of the headers of the loop is the FirstBB.
// Loop is set to true upon successfully detecting such a loop.
void ExprBuilder::checkIrreducibleCFG(BasicBlock *BB,
                    BasicBlock *FirstBB,
                    std::unordered_set<const BasicBlock *> &VisitedBBs,
                    bool &Loop) {
  VisitedBBs.insert(BB);
  for (succ_iterator PI = succ_begin(BB), E = succ_end(BB);
       PI != E; ++PI) {
    BasicBlock *Succ = *PI;
    if (Succ == FirstBB) {
      Loop = true;
    }
    else if (VisitedBBs.count(Succ)) {
      continue;
    }
    else {
      checkIrreducibleCFG(Succ, FirstBB, VisitedBBs, Loop);
    }
    if (Loop)
      return;
  }
}

// Return true if the Basic Block containing the passed Phi node is
// a loop entry point, either the loop header for a natural loop or
// a entry point for an irreducible CFG.
bool ExprBuilder::isLoopEntryPoint(PHINode *Phi) {
  BasicBlock *BB = Phi->getParent();
  // If LLVM can determine if BB is a loop header, simply return true.
  // Presumably, this should handle structured loops.
  if (LI->isLoopHeader(BB))
    return true;
  if (Phi->getNumIncomingValues() <= 1)
    return false;

  bool Loop = false;
  std::unordered_set<const llvm::BasicBlock *> VisitedBBs;
  checkIrreducibleCFG(BB, BB, VisitedBBs, Loop);
  return Loop;
}

Inst *ExprBuilder::makeArrayRead(Value *V) {
  StringRef Name;
  if (Opts.NamedArrays)
    Name = V->getName();
  unsigned Width = DL.getTypeSizeInBits(V->getType());
  APInt KnownZero(Width, 0, false), KnownOne(Width, 0, false);
  bool NonZero = false, NonNegative = false, PowOfTwo = false, Negative = false;
  if (HarvestKnownBits)
    if (V->getType()->isIntOrIntVectorTy() ||
        V->getType()->getScalarType()->isPointerTy()) {
      computeKnownBits(V, KnownZero, KnownOne, DL);
      NonZero = isKnownNonZero(V, DL);
      NonNegative = isKnownNonNegative(V, DL);
      PowOfTwo = isKnownToBeAPowerOfTwo(V, DL);
      Negative = isKnownNegative(V, DL);
    }
  return IC.createVar(Width, Name, KnownZero, KnownOne, NonZero, NonNegative,
                      PowOfTwo, Negative);
}

Inst *ExprBuilder::buildConstant(Constant *c) {
  if (auto ci = dyn_cast<ConstantInt>(c)) {
    return IC.getConst(ci->getValue());
  } else if (auto cf = dyn_cast<ConstantFP>(c)) {
    return IC.getConst(cf->getValueAPF().bitcastToAPInt());
  } else if (isa<ConstantPointerNull>(c) || isa<UndefValue>(c) ||
             isa<ConstantAggregateZero>(c)) {
    return IC.getConst(APInt(DL.getTypeSizeInBits(c->getType()), 0));
  } else {
    // Constant{Expr, Vector, DataSequential, Struct, Array}
    return makeArrayRead(c);
  }
}

Inst *ExprBuilder::buildGEP(Inst *Ptr, gep_type_iterator begin,
                            gep_type_iterator end) {
  unsigned PSize = DL.getPointerSizeInBits();
  for (auto i = begin; i != end; ++i) {
    if (StructType *ST = i.getStructTypeOrNull()) {
      const StructLayout *SL = DL.getStructLayout(ST);
      ConstantInt *CI = cast<ConstantInt>(i.getOperand());
      uint64_t Addend = SL->getElementOffset((unsigned) CI->getZExtValue());
      if (Addend != 0) {
        Ptr = IC.getInst(Inst::Add, PSize,
                         {Ptr, IC.getConst(APInt(PSize, Addend))});
      }
    } else {
      SequentialType *SET = cast<SequentialType>(i.getIndexedType());
      uint64_t ElementSize =
        DL.getTypeStoreSize(SET->getElementType());
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
    unsigned DestSize = DL.getTypeSizeInBits(Cast->getType());

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
    // TODO: replace with a GEP instruction
    //return buildGEP(get(GEP->getOperand(0)), gep_type_begin(GEP),
    //                gep_type_end(GEP));
    return makeArrayRead(V);
  } else if (auto Phi = dyn_cast<PHINode>(V)) {
    // We can't look through phi nodes in loop headers because we might
    // encounter a previous iteration of an instruction and get a wrong result.
    // TODO: In principle we could track loop iterations and maybe even maintain
    // a separate set of values for each iteration (as in bounded model
    // checking).
    if (!isLoopEntryPoint(Phi)) {
      BasicBlock *BB = Phi->getParent();
      BlockInfo &BI = EBC.BlockMap[BB];
      if (!BI.B) {
        std::copy(Phi->block_begin(), Phi->block_end(),
                  std::back_inserter(BI.Preds));
        BI.B = IC.createBlock(BI.Preds.size());
      }
      std::vector<Inst *> Incomings;
      for (auto Pred : BI.Preds) {
        Incomings.push_back(get(Phi->getIncomingValueForBlock(Pred)));
      }
      return IC.getPhi(BI.B, Incomings);
    }
  } else if (auto EV = dyn_cast<ExtractValueInst>(V)) {
    Inst *L = get(EV->getOperand(0));
    ArrayRef<unsigned> Idx = EV->getIndices();
    // NOTE: extractvalue instruction can take set of indices. Most of the
    // times we just pass one index value, i.e. why I am extracting 0th index
    // always, Idx[0]. If required, we can fix it by iterating over set of
    // indices. Additonally, we harvest extractvalue instruction only if the
    // extracted value comes from an overflow instruction. Otherwise, we simply
    // create a var.
    Inst *R = IC.getConst(APInt(32, Idx[0]));
    switch (L->K) {
      default:
        return makeArrayRead(V);
      case Inst::SAddWithOverflow:
      case Inst::UAddWithOverflow:
      case Inst::SSubWithOverflow:
      case Inst::USubWithOverflow:
      case Inst::SMulWithOverflow:
      case Inst::UMulWithOverflow: {
        unsigned WidthExtracted = L->Ops[Idx[0]]->Width;
        return IC.getInst(Inst::ExtractValue, WidthExtracted, {L, R});
      }
    }
  } else if (auto Call = dyn_cast<CallInst>(V)) {
    if (auto II = dyn_cast<IntrinsicInst>(Call)) {
      Inst *L = get(II->getOperand(0));
      switch (II->getIntrinsicID()) {
        default:
          break;
        case Intrinsic::ctpop:
          return IC.getInst(Inst::CtPop, L->Width, {L});
        case Intrinsic::bswap:
          return IC.getInst(Inst::BSwap, L->Width, {L});
        case Intrinsic::cttz:
          return IC.getInst(Inst::Cttz, L->Width, {L});
        case Intrinsic::ctlz:
          return IC.getInst(Inst::Ctlz, L->Width, {L});
        case Intrinsic::sadd_with_overflow: {
          Inst *R = get(II->getOperand(1));
          Inst *Add = IC.getInst(Inst::Add, L->Width, {L, R});
          Inst *Overflow = IC.getInst(Inst::SAddO, 1, {L, R});
          return IC.getInst(Inst::SAddWithOverflow, L->Width+1, {Add, Overflow});
        }
        case Intrinsic::uadd_with_overflow: {
          Inst *R = get(II->getOperand(1));
          Inst *Add = IC.getInst(Inst::Add, L->Width, {L, R});
          Inst *Overflow = IC.getInst(Inst::UAddO, 1, {L, R});
          return IC.getInst(Inst::UAddWithOverflow, L->Width+1, {Add, Overflow});
        }
        case Intrinsic::ssub_with_overflow: {
          Inst *R = get(II->getOperand(1));
          Inst *Sub = IC.getInst(Inst::Sub, L->Width, {L, R});
          Inst *Overflow = IC.getInst(Inst::SSubO, 1, {L, R});
          return IC.getInst(Inst::SSubWithOverflow, L->Width+1, {Sub, Overflow});
        }
        case Intrinsic::usub_with_overflow: {
          Inst *R = get(II->getOperand(1));
          Inst *Sub = IC.getInst(Inst::Sub, L->Width, {L, R});
          Inst *Overflow = IC.getInst(Inst::USubO, 1, {L, R});
          return IC.getInst(Inst::USubWithOverflow, L->Width+1, {Sub, Overflow});
        }
        case Intrinsic::smul_with_overflow: {
          Inst *R = get(II->getOperand(1));
          Inst *Mul = IC.getInst(Inst::Mul, L->Width, {L, R});
          Inst *Overflow = IC.getInst(Inst::SMulO, 1, {L, R});
          return IC.getInst(Inst::SMulWithOverflow, L->Width+1, {Mul, Overflow});
        }
        case Intrinsic::umul_with_overflow: {
          Inst *R = get(II->getOperand(1));
          Inst *Mul = IC.getInst(Inst::Mul, L->Width, {L, R});
          Inst *Overflow = IC.getInst(Inst::UMulO, 1, {L, R});
          return IC.getInst(Inst::UMulWithOverflow, L->Width+1, {Mul, Overflow});
        }
      }
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

void ExprBuilder::addPC(BasicBlock *BB, BasicBlock *Pred,
                        std::vector<InstMapping> &PCs) {
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
      Inst *DI = IC.getConst(APInt(1, true));
      for (auto I = Switch->case_begin(), E = Switch->case_end(); I != E;
           ++I) {
        Inst *CI = IC.getInst(Inst::Ne, 1, {Cond, get(I.getCaseValue())});
        emplace_back_dedup(PCs, CI, DI);
      }
    }
  }
}

// Collect path conditions for a basic block. 
// There are two kinds of path conditions, which correspond to
// two Souper instruction kinds, pc and blockpc, respectively.
// (1) The PC condition is added when the given predecessor of
//     the given basic block is chosen. For example, given the
//     simple CFG below:
//          B1
//         /  \
//       B2    B3
//     The path conditions [B1->B2] and [B1->B3] will be added 
//     to B2 and B3's PCs, respectively, because B1 can determine 
//     the choice of either B2 or B3.
// (2) The BlockPC condition handles cases where we don't know
//     which predecessor would be chosen. In this case, we recursively
//     process each predecessor of the given basic block until
//     the PC condition (as defined above) is met. Then we add
//     the this condition into the BlockPCs of the basic block from
//     which we start our recursion. If a loop head or an entry
//     point of an irreducible loop is encountered along any path, 
//     we stash the collected BlockPCs (if there is any) and return.
//     The following examples (without loops) describe the idea. 
//     A simple example:
//         B1
//        /  \
//            B2  B3
//             \  /
//              B4
//     Suppose we are collecting BlockPCs for B4. Either
//     B2 or B3 can reach B4, we cannot add them into B4's PC.
//     Instead, we add whatever path conditions dominating B2 and
//     B3 into B4's BlockPCs. In this simple case, we will have
//     blockpc %B4, 0, s1, r1 // B1->B2
//      
// Now consider a more complex example:
//            B1          B2
//           /  \        /  \
//               B3    B4
//                \    /
//                  B5        B6
//                 /  \      /  \
//                    B7   B8
//                   /  \  /
//                       B9
// Suppose we are dealing with an instruction %i in B9, After we iterate all
// basic blocks (in ExtractExprCandidates), we will have something like these:
// blockpc %B5, 0, s1, r1 // B1->B3
// blockpc %B5, 1, s2, r2 // B2->B4
// ...
// blockpc %B9, 0, s3, r3 // B5 -> B7
// blockpc %B9, 1, s4, r4 // B6 -> B8
// ...
// cand %i, 1
void ExprBuilder::addPathConditions(BlockPCs &BPCs,
                                    std::vector<InstMapping> &PCs,
                                    std::unordered_set<Block *> &VisitedBlocks,
                                    BasicBlock *BB) {
  if (auto Pred = BB->getSinglePredecessor()) {
    addPathConditions(BPCs, PCs, VisitedBlocks, Pred);
    addPC(BB, Pred, PCs);
  } else if (ExploitBPCs) {
    // BB is the entry of the function.
    if (pred_begin(BB) == pred_end(BB))
      return;

    BlockInfo &BI = EBC.BlockMap[BB];
    // We encounter a loop entry point.
    // FIXME: Basically, we stop at this point, i.e., we don't collect
    // BlockPC's before loops. Maybe we should consider to resume from BB's
    // immediate dominator. If we do, generate another pull request for this
    // purpose.
    if (!BI.B)
      return;

    // It's possible that we will re-visit a Block, e.g.,
    //          \  /
    //           B1
    //          /  \
    //         B2  B3
    //          \  /
    //           B4
    if (VisitedBlocks.count(BI.B))
      return;

    VisitedBlocks.insert(BI.B);
    for (unsigned i = 0; i < BI.Preds.size(); ++i) {
      auto Pred = BI.Preds[i];
      std::vector<InstMapping> PCs;
      if (Pred->getSinglePredecessor()) {
        addPathConditions(BPCs, PCs, VisitedBlocks, Pred);
      }
      else {
        // In case the predecessor is a br or switch instruction.
        addPC(BB, Pred, PCs);
      }
      for (auto PC : PCs)
        BPCs.emplace_back(BlockPCMapping(BI.B, i, PC));
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

// Similar to AddPCSets, add the variable sets of BlockPCs as equivalence
// classes to Vars. Return a BPCSets of the same size as BPCs.
std::vector<Inst *> AddBlockPCSets(const BlockPCs &BPCs, InstClasses &Vars) {
  std::vector<Inst *> BPCSets(BPCs.size());
  for (unsigned i = 0; i != BPCs.size(); ++i) {
    llvm::DenseSet<Inst *> SeenInsts;
    auto BPCLeader =
        AddVarSet(Vars.member_end(), Vars, SeenInsts, BPCs[i].PC.LHS);
    if (BPCLeader != Vars.member_end())
      BPCSets[i] = *BPCLeader;
  }
  return BPCSets;
}

// Return vectors of relevant BlockPCs and PCs for a candidate, namely those
// whose variable sets are in the same equivalence class as the candidate's.
std::tuple<BlockPCs, std::vector<InstMapping>> GetRelevantPCs(
    const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
    const std::vector<Inst *> &BPCSets, const std::vector<Inst *> &PCSets,
    InstClasses Vars, InstMapping Cand) {

  llvm::DenseSet<Inst *> SeenInsts;
  auto Leader = AddVarSet(Vars.member_end(), Vars, SeenInsts, Cand.LHS);

  BlockPCs RelevantBPCs;
  for (unsigned i = 0; i != BPCs.size(); ++i) {
    if (BPCSets[i] != 0 && Vars.findLeader(BPCSets[i]) == Leader)
      RelevantBPCs.emplace_back(BPCs[i]);
  }

  std::vector<InstMapping> RelevantPCs;
  for (unsigned i = 0; i != PCs.size(); ++i) {
    if (PCSets[i] != 0 && Vars.findLeader(PCSets[i]) == Leader)
      RelevantPCs.emplace_back(PCs[i]);
  }
  return std::make_tuple(RelevantBPCs, RelevantPCs);
}

void ExtractExprCandidates(Function &F, const LoopInfo *LI,
                           const ExprBuilderOptions &Opts, InstContext &IC,
                           ExprBuilderContext &EBC,
                           FunctionCandidateSet &Result) {
  ExprBuilder EB(Opts, F.getParent(), LI, IC, EBC);

  for (auto &BB : F) {
    std::unique_ptr<BlockCandidateSet> BCS(new BlockCandidateSet);
    for (auto &I : BB) {
      if (I.getType()->isIntegerTy())
        BCS->Replacements.emplace_back(&I, InstMapping(EB.get(&I), 0));
    }
    if (!BCS->Replacements.empty()) {
      std::unordered_set<Block *> VisitedBlocks;
      EB.addPathConditions(BCS->BPCs, BCS->PCs, VisitedBlocks, &BB);

      InstClasses Vars, BPCVars;
      auto PCSets = AddPCSets(BCS->PCs, Vars);
      auto BPCSets = AddBlockPCSets(BCS->BPCs, BPCVars);

      for (auto &R : BCS->Replacements) {
        std::tie(R.BPCs, R.PCs) = 
          GetRelevantPCs(BCS->BPCs, BCS->PCs, BPCSets, PCSets, Vars, R.Mapping);
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
    Info.addRequired<LoopInfoWrapperPass>();
    Info.setPreservesAll();
  }

  bool runOnFunction(Function &F) {
    LoopInfo *LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
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

  legacy::FunctionPassManager FPM(F->getParent());
  FPM.add(new ExtractExprCandidatesPass(Opts, IC, EBC, Result));
  FPM.run(*F);

  return Result;
}
