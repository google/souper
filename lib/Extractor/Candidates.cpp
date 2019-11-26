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

#include "llvm/ADT/EquivalenceClasses.h"
#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/KnownBits.h"
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
static llvm::cl::opt<bool> HarvestDataFlowFacts(
    "souper-harvest-dataflow-facts",
    llvm::cl::desc("Perform data flow analysis (default=true)"),
    llvm::cl::init(true));
static llvm::cl::opt<bool> HarvestUses(
    "souper-harvest-uses",
    llvm::cl::desc("Harvest operands (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintNegAtReturn(
    "print-neg-at-return",
    llvm::cl::desc("Print negative dfa in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintNonNegAtReturn(
    "print-nonneg-at-return",
    llvm::cl::desc("Print non-negative dfa in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintKnownAtReturn(
    "print-known-at-return",
    llvm::cl::desc("Print known bits in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintPowerTwoAtReturn(
    "print-power-two-at-return",
    llvm::cl::desc("Print power two dfa in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintNonZeroAtReturn(
    "print-non-zero-at-return",
    llvm::cl::desc("Print non zero dfa in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintSignBitsAtReturn(
    "print-sign-bits-at-return",
    llvm::cl::desc("Print sign bits dfa in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintRangeAtReturn(
    "print-range-at-return",
    llvm::cl::desc("Print range inforation in each value returned from a function (default=false)"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> PrintDemandedBitsAtReturn(
    "print-demanded-bits-from-harvester",
    llvm::cl::desc("Print demanded bits (default=false)"),
    llvm::cl::init(false));

extern bool UseAlive;

using namespace llvm;
using namespace souper;

// LLVM removes this API from version 9, this function is copied from LLVM 8
llvm::APInt souper::getSetSize(const llvm::ConstantRange &R) {
  if (R.isFullSet()) {
    APInt Size(R.getBitWidth()+1, 0);
    Size.setBit(R.getBitWidth());
    return Size;
  }

  // This is also correct for wrapped sets.
  return (R.getUpper() - R.getLower()).zext(R.getBitWidth()+1);
}

void CandidateReplacement::printFunction(llvm::raw_ostream &Out) const {
  assert(Mapping.LHS->hasOrigin(Origin));
  const Function *F = Origin->getParent()->getParent();
  std::string N;
  if (F->hasLocalLinkage()) {
    N = (F->getParent()->getModuleIdentifier() + ":" + F->getName()).str();
  } else {
    N = F->getName();
  }
  Out << "; Function: " << N << '\n';
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
              DemandedBits *DB, LazyValueInfo *LVI, ScalarEvolution *SE,
              TargetLibraryInfo * TLI, InstContext &IC,
              ExprBuilderContext &EBC)
    : Opts(Opts), DL(M->getDataLayout()), LI(LI), DB(DB), LVI(LVI), SE(SE), TLI(TLI), IC(IC), EBC(EBC) {}

  const ExprBuilderOptions &Opts;
  const DataLayout &DL;
  const LoopInfo *LI;
  DemandedBits *DB;
  LazyValueInfo *LVI;
  ScalarEvolution *SE;
  TargetLibraryInfo *TLI;
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
  Inst *build(Value *V, APInt DemandedBits);
  Inst *buildHelper(Value *V);
  void addPC(BasicBlock *BB, BasicBlock *Pred, std::vector<InstMapping> &PCs);
  void addPathConditions(BlockPCs &BPCs, std::vector<InstMapping> &PCs,
                         std::unordered_set<Block *> &VisitedBlocks,
                         BasicBlock *BB);
  Inst *get(Value *V, APInt DemandedBits);
  Inst *get(Value *V);
  Inst *getFromUse(Value *V);
  void markExternalUses(Inst *I);
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
  KnownBits Known(Width);
  bool NonZero = false, NonNegative = false, PowOfTwo = false, Negative = false;
  unsigned NumSignBits = 1;
  ConstantRange Range = llvm::ConstantRange(Width, /*isFullSet=*/true);
  if (HarvestDataFlowFacts) {
    if (V->getType()->isIntOrIntVectorTy(Width) ||
        V->getType()->isPtrOrPtrVectorTy()) {
      computeKnownBits(V, Known, DL);
      NonZero = isKnownNonZero(V, DL);
      NonNegative = isKnownNonNegative(V, DL);
      PowOfTwo = isKnownToBeAPowerOfTwo(V, DL);
      Negative = isKnownNegative(V, DL);
      NumSignBits = ComputeNumSignBits(V, DL);
    }

    if (V->getType()->isIntegerTy()) {
      if (Instruction *I = dyn_cast<Instruction>(V)) {
        // TODO: Find out a better way to get the current basic block
        // with this approach, we might be restricting the constant
        // range harvesting. Because range info. might be coming from
        // llvm values other than instruction.
        BasicBlock *BB = I->getParent();
        auto LVIRange = LVI->getConstantRange(V, BB);
        auto SC = SE->getSCEV(V);
        auto R1 = LVIRange.intersectWith(SE->getSignedRange(SC));
        auto R2 = LVIRange.intersectWith(SE->getUnsignedRange(SC));
        Range = getSetSize(R1).ult(getSetSize(R2)) ? R1 : R2;
      }
    }
  }

  return IC.createVar(Width, Name, Range, Known.Zero, Known.One, NonZero, NonNegative,
                      PowOfTwo, Negative, NumSignBits, 0);
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

void ExprBuilder::markExternalUses (Inst *I) {
  std::map<Inst *, unsigned> UsesCount;
  std::unordered_set<Inst *> Visited;
  std::vector<Inst *> Stack;
  Stack.push_back(I);
  while(!Stack.empty()) {
    Inst* T = Stack.back();
    Stack.pop_back();
    for (auto Op: T->Ops) {
      if (Op->K != Inst::Const && Op->K != Inst::Var
          && Op->K != Inst::UntypedConst && Op->K != Inst::Phi) {

        if (UsesCount.find(Op) == UsesCount.end())
          UsesCount[Op] = 1;
        else
          UsesCount[Op]++;

        if (Visited.insert(Op).second) {
          Stack.push_back(Op);
        }
      }
    }
  }
  for (auto U : UsesCount)
    for (auto R : EBC.InstMap)
      if (R.second == U.first && R.first->getNumUses() != U.second)
        I->DepsWithExternalUses.insert(U.first);
}

Inst *ExprBuilder::build(Value *V, APInt DemandedBits) {
  Inst *I = buildHelper(V);
  I->DemandedBits = DemandedBits;
  return I;
}

Inst *ExprBuilder::buildHelper(Value *V) {
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
    if (UseAlive) { // FIXME: Remove this after alive supports phi
      return makeArrayRead(V);
    }
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
  } else if (auto FI = dyn_cast<FreezeInst>(V)) {
    Inst *Op0 = get(FI->getOperand(0));
    return IC.getInst(Inst::Freeze, Op0->Width, {Op0});
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
    LibFunc Func;
    if (auto II = dyn_cast<IntrinsicInst>(Call)) {
      Inst *L = get(II->getOperand(0));
      Inst *R = nullptr;
      if(II->getNumOperands() > 1) R = get(II->getOperand(1));
      switch (II->getIntrinsicID()) {
        default:
          break;
        case Intrinsic::ctpop:
          return IC.getInst(Inst::CtPop, L->Width, {L});
        case Intrinsic::bswap:
          return IC.getInst(Inst::BSwap, L->Width, {L});
        case Intrinsic::bitreverse:
          return IC.getInst(Inst::BitReverse, L->Width, {L});
        case Intrinsic::cttz:
          return IC.getInst(Inst::Cttz, L->Width, {L});
        case Intrinsic::ctlz:
          return IC.getInst(Inst::Ctlz, L->Width, {L});
        case Intrinsic::fshl:
        case Intrinsic::fshr: {
          Inst *ShAmt = get(II->getOperand(2));
          Inst::Kind K =
              II->getIntrinsicID() == Intrinsic::fshl ? Inst::FShl : Inst::FShr;
          return IC.getInst(K, L->Width, {/*High=*/L, /*Low=*/R, ShAmt});
        }
        case Intrinsic::sadd_with_overflow: {
          Inst *Add = IC.getInst(Inst::Add, L->Width, {L, R}, /*Available=*/false);
          Inst *Overflow = IC.getInst(Inst::SAddO, 1, {L, R}, /*Available=*/false);
          return IC.getInst(Inst::SAddWithOverflow, L->Width+1, {Add, Overflow});
        }
        case Intrinsic::uadd_with_overflow: {
          Inst *Add = IC.getInst(Inst::Add, L->Width, {L, R}, /*Available=*/false);
          Inst *Overflow = IC.getInst(Inst::UAddO, 1, {L, R}, /*Available=*/false);
          return IC.getInst(Inst::UAddWithOverflow, L->Width+1, {Add, Overflow});
        }
        case Intrinsic::ssub_with_overflow: {
          Inst *Sub = IC.getInst(Inst::Sub, L->Width, {L, R}, /*Available=*/false);
          Inst *Overflow = IC.getInst(Inst::SSubO, 1, {L, R}, /*Available=*/false);
          return IC.getInst(Inst::SSubWithOverflow, L->Width+1, {Sub, Overflow});
        }
        case Intrinsic::usub_with_overflow: {
          Inst *Sub = IC.getInst(Inst::Sub, L->Width, {L, R}, /*Available=*/false);
          Inst *Overflow = IC.getInst(Inst::USubO, 1, {L, R}, /*Available=*/false);
          return IC.getInst(Inst::USubWithOverflow, L->Width+1, {Sub, Overflow});
        }
        case Intrinsic::smul_with_overflow: {
          Inst *Mul = IC.getInst(Inst::Mul, L->Width, {L, R}, /*Available=*/false);
          Inst *Overflow = IC.getInst(Inst::SMulO, 1, {L, R}, /*Available=*/false);
          return IC.getInst(Inst::SMulWithOverflow, L->Width+1, {Mul, Overflow});
        }
        case Intrinsic::umul_with_overflow: {
          Inst *Mul = IC.getInst(Inst::Mul, L->Width, {L, R}, /*Available=*/false);
          Inst *Overflow = IC.getInst(Inst::UMulO, 1, {L, R}, /*Available=*/false);
          return IC.getInst(Inst::UMulWithOverflow, L->Width+1, {Mul, Overflow});
        }
        case Intrinsic::sadd_sat: {
          return IC.getInst(Inst::SAddSat, L->Width, {L, R});
        }
        case Intrinsic::uadd_sat: {
          return IC.getInst(Inst::UAddSat, L->Width, {L, R});
        }
        case Intrinsic::ssub_sat: {
          return IC.getInst(Inst::SSubSat, L->Width, {L, R});
        }
        case Intrinsic::usub_sat: {
          return IC.getInst(Inst::USubSat, L->Width, {L, R});
        }
      }
    } else {
      Function* F = Call->getCalledFunction();
      if(F && TLI->getLibFunc(*F, Func) && TLI->has(Func)) {
        switch (Func) {
          case LibFunc_abs: {
            Inst *A = get(Call->getOperand(0));
            Inst *Z = IC.getConst(APInt(A->Width, 0));
            Inst *NegA = IC.getInst(Inst::SubNSW, A->Width, {Z, A}, /*Available=*/false);
            Inst *Cmp = IC.getInst(Inst::Slt, 1, {Z, A}, /*Available=*/false);
            return IC.getInst(Inst::Select, A->Width, {Cmp, A, NegA});
          }
          default:
            break;
        }
      }
    }
  }

  return makeArrayRead(V);
}

Inst *ExprBuilder::get(Value *V, APInt DemandedBits) {
  // Cache V if V is not found in InstMap
  Inst *&E = EBC.InstMap[V];
  if (!E)
    E = build(V, DemandedBits);
  if (E->K != Inst::Const && !E->hasOrigin(V))
    E->Origins.push_back(V);
  return E;
}

Inst *ExprBuilder::getFromUse(Value *V) {
  // Do not find from cache
  unsigned Width = DL.getTypeSizeInBits(V->getType());
  APInt DemandedBits = APInt::getAllOnesValue(Width);
  Inst *E = build(V, DemandedBits);
  if (E->K != Inst::Const && !E->hasOrigin(V))
    E->Origins.push_back(V);
  return E;
}

Inst *ExprBuilder::get(Value *V) {
  // Cache V if V is not found in InstMap
  Inst *&E = EBC.InstMap[V];
  if (!E) {
    unsigned Width = DL.getTypeSizeInBits(V->getType());
    APInt DemandedBits = APInt::getAllOnesValue(Width);
    E = build(V, DemandedBits);
  }
  if (E->K != Inst::Const && !E->hasOrigin(V))
    E->Origins.push_back(V);
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
        Inst *CI = IC.getInst(Inst::Ne, 1, {Cond, get(I->getCaseValue())});
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
      // In case the predecessor is a br or switch instruction.
      addPC(BB, Pred, PCs);
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

std::string convertBoolToStr(bool b) {
  return b ? "true" : "false";
}

void PrintDataflowInfo(Function &F, Instruction &I, LazyValueInfo *LVI,
                       ScalarEvolution *SE) {
  if (I.getNumOperands() == 0) {
    return;
  }
  auto V = I.getOperand(0);
  auto DL = F.getParent()->getDataLayout();
  if (PrintNegAtReturn) {
    bool Negative = isKnownNegative(V, DL);
    llvm::outs() << "negative from compiler: " << convertBoolToStr(Negative)
                 << "\n";
  }
  if (PrintNonNegAtReturn) {
    bool NonNegative = isKnownNonNegative(V, DL);
    llvm::outs() << "nonNegative from compiler: "
                 << convertBoolToStr(NonNegative) << "\n";
  }
  if (PrintKnownAtReturn) {
    unsigned Width = DL.getTypeSizeInBits(V->getType());
    KnownBits Known(Width);
    computeKnownBits(V, Known, DL);
    llvm::outs() << "known bits from compiler: "
                 << Inst::getKnownBitsString(Known.Zero, Known.One) << "\n";
  }
  if (PrintPowerTwoAtReturn) {
    bool PowerTwo = isKnownToBeAPowerOfTwo(V, DL);
    llvm::outs() << "powerOfTwo from compiler: " << convertBoolToStr(PowerTwo)
                 << "\n";
  }
  if (PrintNonZeroAtReturn) {
    bool NonZero = isKnownNonZero(V, DL);
    llvm::outs() << "nonZero from compiler: " << convertBoolToStr(NonZero)
                 << "\n";
  }
  if (PrintSignBitsAtReturn) {
    unsigned NumSignBits = ComputeNumSignBits(V, DL);
    llvm::outs() << "signBits from compiler: " << NumSignBits << "\n";
  }
  if (PrintRangeAtReturn) {
    unsigned Width = DL.getTypeSizeInBits(V->getType());
    ConstantRange Range = llvm::ConstantRange(Width, /*isFullSet=*/true);
    if (V->getType()->isIntegerTy()) {
      if (Instruction *I = dyn_cast<Instruction>(V)) {
        BasicBlock *BB = I->getParent();
        auto LVIRange = LVI->getConstantRange(V, BB);
        auto SC = SE->getSCEV(V);
        auto R1 = LVIRange.intersectWith(SE->getSignedRange(SC));
        auto R2 = LVIRange.intersectWith(SE->getUnsignedRange(SC));
        Range = getSetSize(R1).ult(getSetSize(R2)) ? R1 : R2;
      }
    }
    llvm::outs() << "range from compiler: [" << Range.getLower() << ","
                 << Range.getUpper() << ")" << "\n";
  }
}

void ExtractExprCandidates(Function &F, const LoopInfo *LI, DemandedBits *DB,
                           LazyValueInfo *LVI, ScalarEvolution *SE,
                           TargetLibraryInfo *TLI,
                           const ExprBuilderOptions &Opts, InstContext &IC,
                           ExprBuilderContext &EBC,
                           FunctionCandidateSet &Result) {
  ExprBuilder EB(Opts, F.getParent(), LI, DB, LVI, SE, TLI, IC, EBC);

  for (auto &BB : F) {
    std::unique_ptr<BlockCandidateSet> BCS(new BlockCandidateSet);
    for (auto &I : BB) {
      if (isa<ReturnInst>(I))
        PrintDataflowInfo(F, I, LVI, SE);

      // Note: Demanded bits is a backward dataflow analysis and it computes
      // the dataflow fact at the leaves of a DAG (input parameters of a given
      // function). The API 'getDemandedBits' computes demanded bits for an
      // LLVM instruction, and to avoid computation of this fact for each
      // instruction, we special-cased it to only compute demanded bits for an
      // "addition of input variable with zero" (add x, 0) instruction only.
      // This is just a hack to make it work.
      if (PrintDemandedBitsAtReturn && I.getType()->isIntegerTy()) {
        if (auto BO = dyn_cast<BinaryOperator>(&I)) {
          auto AddOp = BO->getOpcode();
          if (AddOp == Instruction::Add) {
            if (auto ConstZeroOp = dyn_cast<ConstantInt>(BO->getOperand(1))) {
              if (ConstZeroOp->isZero()) {
                APInt DemandedBitsVal = DB->getDemandedBits(&I);
                llvm::outs() << "demanded-bits from compiler for "
                             << I.getName() << " : "
                             << Inst::getDemandedBitsString(DemandedBitsVal)
                             << "\n";
              }
            }
          }
        }
      }

      // Harvest Uses (Operands)
      if (HarvestUses) {
        std::unordered_set<llvm::Instruction *> Visited;
        for (auto &Op : I.operands()) {
          // TODO: support regular values
          if (auto U = dyn_cast<Instruction>(Op)){
            // If uses are in the same block with its def, give up
            if (U->getParent() == &BB)
              continue;
            if (U->getType()->isIntegerTy()) {
              if(Visited.insert(U).second) {
                Inst *In = EB.getFromUse(U);
                In->HarvestKind = HarvestType::HarvestedFromUse;
                In->HarvestFrom = &BB;
                EB.markExternalUses(In);
                BCS->Replacements.emplace_back(U, InstMapping(In, 0));
                assert(EB.get(U)->hasOrigin(U));
              }
            }
          }
        }
      }

      // Harvest Defs
      if (!I.getType()->isIntegerTy())
        continue;
      if (I.hasNUses(0))
        continue;
      Inst *In;
      if (HarvestDataFlowFacts) {
        APInt DemandedBits = DB->getDemandedBits(&I);
        In = EB.get(&I, DemandedBits);
      } else {
        In = EB.get(&I);
      }
      In->HarvestKind = HarvestType::HarvestedFromDef;
      In->HarvestFrom = nullptr;
      EB.markExternalUses(In);
      BCS->Replacements.emplace_back(&I, InstMapping(In, 0));
      assert(EB.get(&I)->hasOrigin(&I));
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
    Info.addRequired<DemandedBitsWrapperPass>();
    Info.addRequired<TargetLibraryInfoWrapperPass>();
    Info.addRequired<LazyValueInfoWrapperPass>();
    Info.addRequired<ScalarEvolutionWrapperPass>();
    Info.setPreservesAll();
  }

  bool runOnFunction(Function &F) {
    TargetLibraryInfo* TLI = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);
    LoopInfo *LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    if (!LI)
      report_fatal_error("getLoopInfo() failed");
    DemandedBits *DB = &getAnalysis<DemandedBitsWrapperPass>().getDemandedBits();
    if (!DB)
      report_fatal_error("getDemandedBits() failed");
    LazyValueInfo *LVI = &getAnalysis<LazyValueInfoWrapperPass>().getLVI();
    if (!LVI)
      report_fatal_error("getLVI() failed");
    ScalarEvolution *SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
    if (!SE)
      report_fatal_error("getSE() failed");
    ExtractExprCandidates(F, LI, DB, LVI, SE, TLI, Opts, IC, EBC, Result);
    return false;
  }
};

char ExtractExprCandidatesPass::ID = 0;

}

FunctionCandidateSet souper::ExtractCandidatesFromPass(
    Function *F, const LoopInfo *LI, DemandedBits *DB, LazyValueInfo *LVI,
    ScalarEvolution *SE, TargetLibraryInfo *TLI, InstContext &IC,
    ExprBuilderContext &EBC, const ExprBuilderOptions &Opts) {
  FunctionCandidateSet Result;
  ExtractExprCandidates(*F, LI, DB, LVI, SE, TLI, Opts, IC, EBC, Result);
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
