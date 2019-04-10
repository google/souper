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

#include "llvm/Support/CommandLine.h"
#include "souper/Extractor/ExprBuilder.h"

#include <queue>

namespace souper {

static llvm::cl::opt<souper::ExprBuilder::Builder> SMTExprBuilder(
    "souper-smt-expr-builder",
    llvm::cl::Hidden,
    llvm::cl::desc("SMT-LIBv2 expression builder (default=klee)"),
    llvm::cl::values(clEnumValN(souper::ExprBuilder::KLEE, "klee",
                                "Use KLEE's Expr library")),
    llvm::cl::init(souper::ExprBuilder::KLEE));

bool ExprBuilder::getUBPaths(Inst *I, UBPath *Current,
                             std::vector<std::unique_ptr<UBPath>> &Paths,
                             UBPathInstMap &CachedUBPathInsts, unsigned Depth) {
  if (Depth > MAX_PHI_DEPTH)
    return false;

  switch (I->K) {
    default:
      break;

    case Inst::AddNSW:
    case Inst::AddNUW:
    case Inst::AddNW:
    case Inst::SubNSW:
    case Inst::SubNUW:
    case Inst::SubNW:
    case Inst::MulNSW:
    case Inst::MulNUW:
    case Inst::MulNW:
    case Inst::UDiv:
    case Inst::SDiv:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::URem:
    case Inst::SRem:
    case Inst::Shl:
    case Inst::ShlNSW:
    case Inst::ShlNUW:
    case Inst::ShlNW:
    case Inst::LShr:
    case Inst::LShrExact:
    case Inst::AShr:
    case Inst::AShrExact:
      Current->UBInsts.push_back(I);
      break;
  }

  const std::vector<Inst *> &Ops = I->orderedOps();
  if (I->K == Inst::Phi) {
    // Early terminate because this phi has been processed.
    // We will use its cached predicates.
    if (CachedUBPathInsts.count(I))
      return true;
    Current->Insts.push_back(I);
    // Since we treat a select instruction as a phi instruction, it's
    // possible that I->B has been added already.
    if (Current->BlockConstraints.count(I->B))
      return true;
    std::vector<UBPath *> Tmp = { Current };
    // Create copies of the current path
    for (unsigned J = 1; J < Ops.size(); ++J) {
      UBPath *New = new UBPath;
      *New = *Current;
      New->BlockConstraints[I->B] = J;
      Paths.push_back(std::unique_ptr<UBPath>(New));
      Tmp.push_back(New);
    }
    // Original path takes the first branch
    Current->BlockConstraints[I->B] = 0;
    // Continue recursively
    for (unsigned J = 0; J < Ops.size(); ++J) {
      if (!getUBPaths(Ops[J], Tmp[J], Paths, CachedUBPathInsts, Depth + 1))
        return false;
    }
  } else if (I->K == Inst::Select) {
    // Early terminate because this phi has been processed.
    // We will use its cached predicates.
    if (CachedUBPathInsts.count(I))
      return true;
    Current->Insts.push_back(I);
    // Current is the predicate operand branch
    std::vector<UBPath *> Tmp = { Current };
    // True branch
    UBPath *True = new UBPath;
    *True = *Current;
    True->SelectBranches[I] = true;
    Paths.push_back(std::move(std::unique_ptr<UBPath>(True)));
    Tmp.push_back(True);
    // False branch
    UBPath *False = new UBPath;
    *False = *Current;
    False->SelectBranches[I] = false;
    Paths.push_back(std::move(std::unique_ptr<UBPath>(False)));
    Tmp.push_back(False);
    // Continue recursively
    for (unsigned J = 0; J < Ops.size(); ++J) {
      if (!getUBPaths(Ops[J], Tmp[J], Paths, CachedUBPathInsts, Depth + 1))
        return false;
    }
  } else {
    for (unsigned J = 0; J < Ops.size(); ++J) {
      if (!getUBPaths(Ops[J], Current, Paths, CachedUBPathInsts, Depth + 1))
        return false;
    }
  }
  return true;
}

void ExprBuilder::getBlockPCPhiPaths(
    Inst *I, BlockPCPhiPath *Current,
    std::vector<std::unique_ptr<BlockPCPhiPath>> &Paths,
    UBPathInstMap &CachedPhis) {

  const std::vector<Inst *> &Ops = I->orderedOps();
  if (I->K != Inst::Phi) {
    for (unsigned J = 0; J < Ops.size(); ++J)
      getBlockPCPhiPaths(Ops[J], Current, Paths, CachedPhis);
    return;
  }

  // Early terminate because this phi has been processed.
  // We will use its cached predicates.
  if (CachedPhis.count(I))
    return;
  Current->Phis.push_back(I);

  // Since we treat a select instruction as a phi instruction, it's
  // possible that I->B has been added already.
  if (Current->BlockConstraints.count(I->B))
    return;

  std::vector<BlockPCPhiPath *> Tmp = { Current };
  // Create copies of the current path
  for (unsigned J = 1; J < Ops.size(); ++J) {
    BlockPCPhiPath *New = new BlockPCPhiPath;
    *New = *Current;
    New->BlockConstraints[I->B] = J;
    Paths.push_back(std::move(std::unique_ptr<BlockPCPhiPath>(New)));
    Tmp.push_back(New);
  }
  // Original path takes the first branch
  Current->BlockConstraints[I->B] = 0;

  auto PCMap = BlockPCMap.find(I->B);
  if (PCMap != BlockPCMap.end()) {
    for (unsigned J = 0; J < Ops.size(); ++J) {
      auto P = PCMap->second.find(J);
      if (P != PCMap->second.end())
        Tmp[J]->PCs.push_back(P->second);
    }
  }
  // Continue recursively
  for (unsigned J = 0; J < Ops.size(); ++J)
    getBlockPCPhiPaths(Ops[J], Tmp[J], Paths, CachedPhis);
}

Inst *ExprBuilder::createPathPred(
    std::map<Block *, unsigned> &BlockConstraints, Inst* PathInst,
    std::map<Inst *, bool> *SelectBranches) {

  Inst *Pred = LIC->getConst(llvm::APInt(1, true));
  if (PathInst->K == Inst::Phi) {
    unsigned Num = BlockConstraints[PathInst->B];
    const auto &PredExpr = PathInst->B->PredVars;
    // Sanity checks
    assert(PredExpr.size() && "there must be path predicates for the UBs");
    assert(PredExpr.size() == PathInst->Ops.size()-1 &&
           "phi predicate size mismatch");
    // Add the predicate(s)
    if (Num == 0)
      Pred = LIC->getInst(Inst::And, 1, {Pred, PredExpr[0]});
    else {
      Inst *Zero = LIC->getConst(llvm::APInt(PredExpr[Num-1]->Width, 0));
      Inst *IsZero = LIC->getInst(Inst::Eq, 1, {PredExpr[Num-1], Zero});
      Pred = LIC->getInst(Inst::And, 1, {Pred, IsZero});
    }
    for (unsigned B = Num; B < PredExpr.size(); ++B)
      Pred = LIC->getInst(Inst::And, 1, {Pred, PredExpr[B]});
  }
  else if (PathInst->K == Inst::Select) {
    Inst *SelectPred = PathInst->orderedOps()[0];
    assert(SelectBranches && "NULL SelectBranches?");
    auto SI = SelectBranches->find(PathInst);
    // The current path doesn't have info about this select instruction.
    if (SI == SelectBranches->end()) {
      return Pred;
    }
    if (SI->second)
      Pred = LIC->getInst(Inst::And, 1, {Pred, SelectPred});
    else {
      Inst *Zero = LIC->getConst(llvm::APInt(SelectPred->Width, 0));
      Inst *IsZero = LIC->getInst(Inst::Eq, 1, {SelectPred, Zero});
      Pred = LIC->getInst(Inst::And, 1, {Pred, IsZero});
    }
  }
  else {
    assert(0 && "cannot reach here");
  }

  return Pred;
}

// Collect UB Inst condition for each Phi instruction.
// The basic algorithm is:
// (1) for a given phi instruction, we first collect all paths that start
//     from the phi. For each path, we also keep the the phi instructions
//     along the path and the UB instructions associated with these phi
//     instructions;
// (2) then for each path, we generate a corresponding expression
//     based on the facts that we collected in step (1), including:
//     * UB instructions;
//     * Phi predicates;
//
// With the algorithm, it is easy to get into the path explosion problem,
// i.e., the number of paths is increased exponentially. Under some
// circumstances, e.g., the number of phis is too large, we will suffer
// with large performance overhead. In some extreme cases, we will fail
// to process some file due to the large memory footprint, i.e., `newing'
// too many UBPaths. Two tricks are used to relief the penalty of the
// path explosion problem:
// (1) caching the expresions for each processed phi, where each
//     expression encodes the path that starts from one of the phi's
//     values. For example, when processing a sample souper IR below
//
//     %0 = block
//     %1 = block
//     %2:i32 = var
//     %3:i32 = shl 0:i32, %2
//     %4:i32 = var
//     %5:i32 = shl 0:i32, %4
//     %6 = var
//     %11:i32 = phi %1, %3, %5
//     %12:i32 = phi %0, %6, %11
//
//     we first encounter phi %11. The generated expression
//     for this phi encodes two paths, i.e., %3 and %5. We cache
//     these two into CachedUBPathInsts. Then we move to process phi %12.
//     At this point, rather than recursively re-contruct %11 (through
//     %12's value), we just re-used the cached path-expressions.
//     For each path-expression, we append it with %12's own predicate
//     and also cache them with %12 for future use. After finishing
//     %12, we will have three entries for phi %12.
// (2) The first trick increases the performance, but we still suffer
//     with large memory consumption, i.e., it's easy to cache too
//     many paths. The second trick is to reduce the memory footprint
//     by only caching "useful" path that has UB Insts. For example,
//     in the example in (1), for phi %12, we don't need to cache
//     the path starting from %6, because this path doesn't have any
//     UB Insts.
//
// These tricks basically relies on the dependency chain of instructions
// generated by souper. For example, if we say %12 depends on %11, then
// %12 would never appear earlier than %11.
Inst *ExprBuilder::getUBInstCondition(Inst *Root) {
  // A map from a Phi instruction to all of its expressions that
  // encode the path and UB Inst predicates.
  UBPathInstMap CachedUBPathInsts;
  std::set<Inst *> UsedUBInsts;
  Inst *Result = LIC->getConst(llvm::APInt(1, true));
  auto UBExprMap = getUBInstConstraints(Root);
  // For each Phi/Select instruction
  for (const auto &I : getUBPathInsts(Root)) {
    if (CachedUBPathInsts.count(I) != 0)
      continue;
    // Recursively collect UB instructions
    // on the block constrained Phi and Select branches
    std::vector<std::unique_ptr<UBPath>> UBPaths;
    UBPath *Current = new UBPath;
    UBPaths.push_back(std::move(std::unique_ptr<UBPath>(Current)));
    if (!getUBPaths(I, Current, UBPaths, CachedUBPathInsts, 0))
      return LIC->getConst(llvm::APInt(1, true));
    CachedUBPathInsts[I] = {};
    // For each found path
    for (const auto &Path : UBPaths) {
      if (!Path->UBInsts.size())
        continue;
      // Aggregate collected UB constraints
      Inst *Ante = LIC->getConst(llvm::APInt(1, true));
      for (const auto &I : Path->UBInsts) {
        auto Iter = UBExprMap.find(I);
        // It's possible that the instruction I is not in the map.
        // For example, it may come from a blockpc which doesn't
        // have any preconditions.
        if (Iter != UBExprMap.end())
          Ante = LIC->getInst(Inst::And, 1, {Ante, Iter->second});
        UsedUBInsts.insert(I);
      }
      // Create path predicate
      Inst *Pred =
        createUBPathInstsPred(I, Path->Insts, Path->BlockConstraints,
                              &Path->SelectBranches, CachedUBPathInsts);
      // Add predicate->UB constraint
      Result = LIC->getInst(Inst::And, 1, {Result, getImpliesInst(Pred, Ante)});
    }
  }
  // Add the unconditional UB constraints at the top level
  for (const auto &Entry: UBExprMap)
    if (!UsedUBInsts.count(Entry.first))
      Result = LIC->getInst(Inst::And, 1, {Result, Entry.second});

  return Result;
}

Inst *ExprBuilder::getDataflowConditions(Inst *I) {
  Inst *Result = LIC->getConst(llvm::APInt(1, true));

  if (I->K != Inst::Var)
    return Result;

  unsigned Width = I->Width;
  Inst *Zero = LIC->getConst(llvm::APInt(Width, 0));
  Inst *One = LIC->getConst(llvm::APInt(Width, 1));

  if (I->KnownZeros.getBoolValue()) {
    Inst *AllOnes = LIC->getConst(llvm::APInt::getAllOnesValue(Width));
    Inst *NotZeros = LIC->getInst(Inst::Xor, Width,
                                  {LIC->getConst(I->KnownZeros), AllOnes});
    Inst *VarNotZero = LIC->getInst(Inst::Or, Width, {I, NotZeros});
    Inst *ZeroBits = LIC->getInst(Inst::Eq, 1, {VarNotZero, NotZeros});
    Result = LIC->getInst(Inst::And, 1, {Result, ZeroBits});
  }
  if (I->KnownOnes.getBoolValue()) {
    Inst *Ones = LIC->getConst(I->KnownOnes);
    Inst *VarAndOnes = LIC->getInst(Inst::And, Width, {I, Ones});
    Inst *OneBits = LIC->getInst(Inst::Eq, 1, {VarAndOnes, Ones});
    Result = LIC->getInst(Inst::And, 1, {Result, OneBits});
  }
  if (I->NonZero) {
    Inst *NonZeroBits = LIC->getInst(Inst::Ne, 1, {I, Zero});
    Result = LIC->getInst(Inst::And, 1, {Result, NonZeroBits});
  }
  if (I->NonNegative) {
    Inst *NonNegBits = LIC->getInst(Inst::Sle, 1, {Zero, I});
    Result = LIC->getInst(Inst::And, 1, {Result, NonNegBits});
  }
  if (I->PowOfTwo) {
    Inst *And = LIC->getInst(Inst::And, Width,
                             {I, LIC->getInst(Inst::Sub, Width, {I, One})});
    Inst *PowerTwoBits = LIC->getInst(Inst::And, 1,
                                      {LIC->getInst(Inst::Ne, 1, {I, Zero}),
                                       LIC->getInst(Inst::Eq, 1, {And, Zero})});
    Result = LIC->getInst(Inst::And, 1, {Result, PowerTwoBits});
  }
  if (I->Negative) {
    Inst *NegBits = LIC->getInst(Inst::Slt, 1, {I, Zero});
    Result = LIC->getInst(Inst::And, 1, {Result, NegBits});
  }
  if (I->NumSignBits > 1) {
    Inst *Diff = LIC->getConst(llvm::APInt(Width, Width - I->NumSignBits));
    Inst *Res = LIC->getInst(Inst::AShr, Width, {I, Diff});
    Diff = LIC->getConst(llvm::APInt(Width, Width-1));
    Inst *TestOnes = LIC->getInst(Inst::AShr, Width,
                                  {LIC->getInst(Inst::Shl, Width, {One, Diff}),
                                   LIC->getConst(llvm::APInt(Width, Width-1))});
    Inst *SignBits = LIC->getInst(Inst::Or, 1,
                                  {LIC->getInst(Inst::Eq, 1, {Res, TestOnes}),
                                   LIC->getInst(Inst::Eq, 1, {Res, Zero})});
    Result = LIC->getInst(Inst::And, 1, {Result, SignBits});
  }
  // TODO: We might want to look into handling empty set later
  if (!I->Range.isEmptySet() && !I->Range.isFullSet()) {
    Inst *Lower = LIC->getConst(I->Range.getLower());
    Inst *Upper = LIC->getConst(I->Range.getUpper());
 
    if (!I->Range.isWrappedSet()) {
      Result = LIC->getInst(Inst::And, 1, {Result, LIC->getInst(Inst::And, 1,
                            {LIC->getInst(Inst::Ule, 1, {Lower, I}),
                             LIC->getInst(Inst::Ult, 1, {I, Upper})})});
    } else {
      Result = LIC->getInst(Inst::And, 1, {Result, LIC->getInst(Inst::Or, 1,
                            {LIC->getInst(Inst::Ule, 1, {Lower, I}),
                             LIC->getInst(Inst::Ult, 1, {I, Upper})})});
    }
  }

  return Result;
}

// Similar to the way we collect UB constraints. We could combine it with 
// getUBInstCondition, because the workflow is quite similar. 
// However, mixing two parts (one for UB constraints, one for BlockPCs)
// may make the code less structured. If we see big performance overhead,
// we may consider to combine these two parts together. 
Inst *ExprBuilder::getBlockPCs(Inst *Root) {

  UBPathInstMap CachedPhis;
  Inst *Result = LIC->getConst(llvm::APInt(1, true));
  // For each Phi instruction
  for (const auto &I : getUBPathInsts(Root)) {
    if (CachedPhis.count(I) != 0)
      continue;
    // Recursively collect BlockPCs
    std::vector<std::unique_ptr<BlockPCPhiPath>> BlockPCPhiPaths;
    BlockPCPhiPath *Current = new BlockPCPhiPath;
    BlockPCPhiPaths.push_back(
                        std::move(std::unique_ptr<BlockPCPhiPath>(Current)));
    getBlockPCPhiPaths(I, Current, BlockPCPhiPaths, CachedPhis);
    CachedPhis[I] = {};
    // For each found path
    for (const auto &Path : BlockPCPhiPaths) {
      if (!Path->PCs.size())
        continue;
      // Aggregate collected BlockPC constraints
      Inst *Ante = LIC->getConst(llvm::APInt(1, true));
      for (const auto &PC : Path->PCs)
        Ante = LIC->getInst(Inst::And, 1, {Ante, PC});
      // Create path predicate
      Inst *Pred =
        createUBPathInstsPred(I, Path->Phis, Path->BlockConstraints,
                              /*SelectBranches=*/nullptr, CachedPhis);
      // Add predicate->UB constraint
      Result = LIC->getInst(Inst::And, 1, {Result, getImpliesInst(Pred, Ante)});
    }
  }

  return Result;
}

void ExprBuilder::setBlockPCMap(const BlockPCs &BPCs) {
  for (auto BPC : BPCs) {
    assert(BPC.B && "Block is NULL!");
    BlockPCPredMap &PCMap = BlockPCMap[BPC.B];
    auto I = PCMap.find(BPC.PredIdx);
    Inst *PE = LIC->getInst(Inst::Eq, 1, {BPC.PC.LHS, BPC.PC.RHS});
    if (I == PCMap.end())
      PCMap[BPC.PredIdx] = PE;
    else
      PCMap[BPC.PredIdx] = LIC->getInst(Inst::And, 1, {I->second, PE});
  }
}

Inst *ExprBuilder::createUBPathInstsPred(
    Inst *CurrentInst, std::vector<Inst *> &PathInsts,
    std::map<Block *, unsigned> &BlockConstraints,
    std::map<Inst *, bool> *SelectBranches, UBPathInstMap &CachedUBPathInsts) {
  Inst *Pred = LIC->getConst(llvm::APInt(1, true));
  for (const auto &PathInst : PathInsts) {
    if (PathInst->Ops.size() == 1)
      continue;
    Inst *InstPred = createPathPred(BlockConstraints, PathInst, SelectBranches);

    UBPathInstMap::iterator PI = CachedUBPathInsts.find(PathInst);
    if (PI == CachedUBPathInsts.end()) {
       CachedUBPathInsts[PathInst] = {};
       PI = CachedUBPathInsts.find(PathInst);
    }
    if (PI->first != CurrentInst && PI->second.size() != 0) {
      // Use cached Expr along each path which has UB Insts,
      // and cache the expanded Expr for the current working Phi
      for (auto CE : PI->second) {
        InstPred = LIC->getInst(Inst::And, 1, {CE, InstPred});
        CachedUBPathInsts[CurrentInst].push_back(InstPred);
        Pred = LIC->getInst(Inst::And, 1, {Pred, InstPred});
      }
    }
    else {
      CachedUBPathInsts[CurrentInst].push_back(InstPred);
      Pred = LIC->getInst(Inst::And, 1, {Pred, InstPred});
    }
  }

  return Pred;
}

Inst *ExprBuilder::getExtractInst(Inst *I, unsigned Offset, unsigned W) {
  if (I->K == Inst::Const || I->K == Inst::UntypedConst) {
    return LIC->getConst(llvm::APInt(I->Val.ashr(Offset)).zextOrTrunc(W));
  } else {
    Inst *AShr = LIC->getInst(Inst::AShr, I->Width,
                              {I, LIC->getConst(llvm::APInt(I->Width, Offset))});
    if (AShr->Width < W)
      return LIC->getInst(Inst::ZExt, W, {AShr});
    else if (AShr->Width > W)
      return LIC->getInst(Inst::Trunc, W, {AShr});
    else
      return AShr;
  }
}

Inst *ExprBuilder::getImpliesInst(Inst *Ante, Inst *I) {
  Inst *Zero = LIC->getConst(llvm::APInt(Ante->Width, 0));
  Inst *IsZero = LIC->getInst(Inst::Eq, 1, {Ante, Zero});
  return LIC->getInst(Inst::Or, 1, {IsZero, I});
}

Inst *ExprBuilder::addnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Add = LIC->getInst(Inst::Add, Width, {L, R});
   Inst *LMSB = getExtractInst(L, Width-1, 1);
   Inst *RMSB = getExtractInst(R, Width-1, 1);
   Inst *AddMSB = getExtractInst(Add, Width-1, 1);
   return getImpliesInst(LIC->getInst(Inst::Eq, 1, {LMSB, RMSB}),
                         LIC->getInst(Inst::Eq, 1, {LMSB, AddMSB}));
}

Inst *ExprBuilder::addnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Lext = LIC->getInst(Inst::ZExt, Width+1, {L});
   Inst *Rext = LIC->getInst(Inst::ZExt, Width+1, {R});
   Inst *Add = LIC->getInst(Inst::Add, Width+1, {Lext, Rext});
   Inst *AddMSB = getExtractInst(Add, Width, 1);
   return LIC->getInst(Inst::Eq, 1,
                       {AddMSB, LIC->getConst(llvm::APInt(1, false))});
}

Inst *ExprBuilder::subnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Sub = LIC->getInst(Inst::Sub, Width, {L, R});
   Inst *LMSB = getExtractInst(L, Width-1, 1);
   Inst *RMSB = getExtractInst(R, Width-1, 1);
   Inst *SubMSB = getExtractInst(Sub, Width-1, 1);
   return getImpliesInst(LIC->getInst(Inst::Ne, 1, {LMSB, RMSB}),
                         LIC->getInst(Inst::Eq, 1, {LMSB, SubMSB}));
}

Inst *ExprBuilder::subnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Lext = LIC->getInst(Inst::ZExt, Width+1, {L});
   Inst *Rext = LIC->getInst(Inst::ZExt, Width+1, {R});
   Inst *Sub = LIC->getInst(Inst::Sub, Width+1, {Lext, Rext});
   Inst *SubMSB = getExtractInst(Sub, Width, 1);
   return LIC->getInst(Inst::Eq, 1,
                       {SubMSB, LIC->getConst(llvm::APInt(1, false))});
}

Inst *ExprBuilder::mulnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   // The computation below has to be performed on the operands of
   // multiplication instruction. The instruction using mulnswUB()
   // can be of different width, for instance in SMulO instruction
   // which is of 1-bit, but the operands width are to be used here.
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   L = LIC->getInst(Inst::SExt, 2*Width, {L});
   R = LIC->getInst(Inst::SExt, 2*Width, {R});
   Inst *Mul = LIC->getInst(Inst::Mul, 2*Width, {L, R});
   Inst *LowerBits = getExtractInst(Mul, 0, Width);
   Inst *LowerBitsExt = LIC->getInst(Inst::SExt, 2*Width, {LowerBits});
   return LIC->getInst(Inst::Eq, 1, {Mul, LowerBitsExt});
}

Inst *ExprBuilder::mulnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Lext = LIC->getInst(Inst::ZExt, 2*Width, {L});
   Inst *Rext = LIC->getInst(Inst::ZExt, 2*Width, {R});
   Inst *Mul = LIC->getInst(Inst::Mul, 2*Width, {Lext, Rext});
   Inst *HigherBits = getExtractInst(Mul, Width, Width);
   return LIC->getInst(Inst::Eq, 1,
                       {HigherBits, LIC->getConst(llvm::APInt(Width, 0))});
}

Inst *ExprBuilder::udivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto R = Ops[1];
   return LIC->getInst(Inst::Ne, 1,
                       {R, LIC->getConst(llvm::APInt(R->Width, 0))});
}

Inst *ExprBuilder::udivExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Udiv = LIC->getInst(Inst::UDiv, Width, {L, R});
   return LIC->getInst(Inst::Eq, 1,
                       {L, LIC->getInst(Inst::Mul, Width, {R, Udiv})});
}

Inst *ExprBuilder::sdivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *ShiftBy = LIC->getConst(llvm::APInt(Width, Width-1));
   Inst *IntMin = LIC->getInst(Inst::Shl, Width,
                               {LIC->getConst(llvm::APInt(Width, 1)), ShiftBy});
   Inst *NegOne = LIC->getInst(Inst::AShr, Width, {IntMin, ShiftBy});
   Inst *NeExpr = LIC->getInst(Inst::Ne, 1,
                               {R, LIC->getConst(llvm::APInt(R->Width, 0))});
   Inst *OrExpr = LIC->getInst(Inst::Or, 1,
                               {LIC->getInst(Inst::Ne, 1, {L, IntMin}),
                                LIC->getInst(Inst::Ne, 1, {R, NegOne})});
   return LIC->getInst(Inst::And, 1, {NeExpr, OrExpr});
}

Inst *ExprBuilder::sdivExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Sdiv = LIC->getInst(Inst::SDiv, Width, {L, R});
   return LIC->getInst(Inst::Eq, 1,
                       {L, LIC->getInst(Inst::Mul, Width, {R, Sdiv})});
}

Inst *ExprBuilder::shiftUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Lwidth = LIC->getConst(llvm::APInt(Width, Width));
   return LIC->getInst(Inst::Ult, 1, {R, Lwidth});
}

Inst *ExprBuilder::shlnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Result = LIC->getInst(Inst::Shl, Width, {L, R});
   Inst *RShift = LIC->getInst(Inst::AShr, Width, {Result, R});
   return LIC->getInst(Inst::Eq, 1, {RShift, L});
}

Inst *ExprBuilder::shlnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Result = LIC->getInst(Inst::Shl, Width, {L, R});
   Inst *RShift = LIC->getInst(Inst::LShr, Width, {Result, R});
   return LIC->getInst(Inst::Eq, 1, {RShift, L});
}

Inst *ExprBuilder::lshrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Result = LIC->getInst(Inst::LShr, Width, {L, R});
   Inst *LShift = LIC->getInst(Inst::Shl, Width, {Result, R});
   return LIC->getInst(Inst::Eq, 1, {LShift, L});
}

Inst *ExprBuilder::ashrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   auto L = Ops[0];
   auto R = Ops[1];
   unsigned Width = L->Width;
   Inst *Result = LIC->getInst(Inst::AShr, Width, {L, R});
   Inst *LShift = LIC->getInst(Inst::Shl, Width, {Result, R});
   return LIC->getInst(Inst::Eq, 1, {LShift, L});
}

std::map<Inst *, Inst *> ExprBuilder::getUBInstConstraints(Inst *Root) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::map<Inst *, Inst *> Result;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    // Collect UB instructions
    switch (I->K) {
    case Inst::AddNSW: {
      Result.emplace(I, addnswUB(I));
      break;
    }
    case Inst::AddNUW: {
      Result.emplace(I, addnuwUB(I));
      break;
    }
    case Inst::AddNW: {
      Result.emplace(I, LIC->getInst(Inst::And, 1,
                                     {addnswUB(I), addnuwUB(I)}));
      break;
    }
    case Inst::SubNSW: {
      Result.emplace(I, subnswUB(I));
      break;
    }
    case Inst::SubNUW: {
      Result.emplace(I, subnuwUB(I));
      break;
    }
    case Inst::SubNW: {
      Result.emplace(I, LIC->getInst(Inst::And, 1,
                                     {subnswUB(I), subnuwUB(I)}));
      break;
    }
    case Inst::MulNSW: {
      Result.emplace(I, mulnswUB(I));
      break;
    }
    case Inst::MulNUW: {
      Result.emplace(I, mulnuwUB(I));
      break;
    }
    case Inst::MulNW: {
      Result.emplace(I, LIC->getInst(Inst::And, 1,
                                     {mulnswUB(I), mulnuwUB(I)}));
      break;
    }

    case Inst::UDiv:
    case Inst::SDiv:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::URem:
    case Inst::SRem: { // Fall-through
      // If the second oprand is 0, then it definitely causes UB.
      // There are a few cases where an expression folds operations into zero,
      // e.g., "sext i16 0 to i32", "0 + 0", "2 - 2", etc.  In all cases,
      // we skip building the corresponding expressions and just return
      // a constant zero.
      Inst *R = I->Ops[1];
      if (R == LIC->getConst(llvm::APInt(R->Width, 0))) {
        Result.emplace(I, LIC->getConst(llvm::APInt(1, false)));
        return Result;
      }

      switch (I->K) {
      default:
        break;

      case Inst::UDiv: {
        Result.emplace(I, udivUB(I));
        break;
      }
      case Inst::SDiv: {
        Result.emplace(I, sdivUB(I));
        break;
      }
      case Inst::UDivExact: {
        Result.emplace(I, LIC->getInst(Inst::And, 1,
                                       {udivUB(I), udivExactUB(I)}));
        break;
      }
      case Inst::SDivExact: {
        Result.emplace(I, LIC->getInst(Inst::And, 1,
                                       {sdivUB(I), sdivExactUB(I)}));
        break;
      }
      case Inst::URem: {
        Result.emplace(I, udivUB(I));
        break;
      }
      case Inst::SRem: {
        Result.emplace(I, sdivUB(I));
        break;
      }
      llvm_unreachable("unknown kind");
      }
    }

    case Inst::Shl: {
      Result.emplace(I, shiftUB(I));
      break;
    }
    case Inst::ShlNSW: {
      Result.emplace(I, LIC->getInst(Inst::And, 1, {shiftUB(I), shlnswUB(I)}));
      break;
    }
    case Inst::ShlNUW: {
      Result.emplace(I, LIC->getInst(Inst::And, 1, {shiftUB(I), shlnuwUB(I)}));
      break;
    }
    case Inst::ShlNW: {
      Inst *nwUB = LIC->getInst(Inst::And, 1, {shlnswUB(I), shlnuwUB(I)});
      Result.emplace(I, LIC->getInst(Inst::And, 1, {shiftUB(I), nwUB}));
      break;
    }
    case Inst::LShr: {
      Result.emplace(I, shiftUB(I));
      break;
    }
    case Inst::LShrExact: {
      Result.emplace(I, LIC->getInst(Inst::And, 1,
                                     {shiftUB(I), lshrExactUB(I)}));
      break;
    }
    case Inst::AShr: {
      Result.emplace(I, shiftUB(I));
      break;
    }
    case Inst::AShrExact: {
      Result.emplace(I, LIC->getInst(Inst::And, 1,
                                     {shiftUB(I), ashrExactUB(I)}));
      break;
    }
    default:
      break;
    }

    if (Visited.insert(I).second)
      for (auto Op : I->orderedOps())
        Q.push(Op);
  }

  return Result;
}

std::vector<Inst *> ExprBuilder::getUBPathInsts(Inst *Root) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::vector<Inst *> Result;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    // Collect UB path instructions
    switch (I->K) {
    case Inst::Phi: {
      Result.push_back(I);
      break;
    }
    case Inst::Select: {
      Result.push_back(I);
      break;
    }
    default:
      break;
    }

    if (Visited.insert(I).second)
      for (auto Op : I->orderedOps())
        Q.push(Op);
  }

  return Result;
}

std::vector<Inst *> ExprBuilder::getVarInsts(const std::vector<Inst *> Insts) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::vector<Inst *> Result;
  std::queue<Inst *> Q;
  // Populate the queue
  for (const auto &I : Insts)
    Q.push(I);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    if (I->K == Inst::Var)
      Result.push_back(I);
    if (Visited.insert(I).second)
      for (auto Op : I->orderedOps())
        Q.push(Op);
  }

  return Result;
}

// Return a candidate which must be proven valid for the candidate to apply.
Inst *ExprBuilder::GetCandidateExprForReplacement(
    const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
    InstMapping Mapping, Inst *Precondition, bool Negate) {
  Inst *Result = nullptr;

  // Build LHS
  Inst *LHS = Mapping.LHS;
  Inst *Ante = LIC->getConst(llvm::APInt(1, true));

  // Get demanded bits
  Inst *DemandedBits = LIC->getConst(LHS->DemandedBits);
  if (!LHS->DemandedBits.isAllOnesValue())
    LHS = LIC->getInst(Inst::And, LHS->Width, {LHS, DemandedBits});

  // Get UB constraints of LHS
  Inst *LHSUB = getUBInstCondition(Mapping.LHS);
  if (LHSUB == LIC->getConst(llvm::APInt(1, false)))
    return nullptr;

  // Build PCs
  for (const auto &PC : PCs) {
    Inst *Eq = LIC->getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = LIC->getInst(Inst::And, 1, {Ante, Eq});
    // Get UB constraints of PC
    LHSUB = LIC->getInst(Inst::And, 1, {LHSUB, getUBInstCondition(Eq)});
  }

  // Build BPCs
  if (BPCs.size()) {
    setBlockPCMap(BPCs);
    Ante = LIC->getInst(Inst::And, 1, {Ante, getBlockPCs(Mapping.LHS)});
  }

  // Build RHS
  Inst *RHS = Mapping.RHS;

  // Get demanded bits
  if (!Mapping.LHS->DemandedBits.isAllOnesValue())
    RHS = LIC->getInst(Inst::And, RHS->Width, {RHS, DemandedBits});

  // Get known bit constraints
  for (const auto &I : getVarInsts({Mapping.LHS, Mapping.RHS}))
    Ante = LIC->getInst(Inst::And, 1, {Ante, getDataflowConditions(I)});

  // Get UB constraints of RHS
  Inst *RHSUB = getUBInstCondition(Mapping.RHS);
  if (RHSUB == LIC->getConst(llvm::APInt(1, false)))
    return nullptr;

  if (Negate) // (LHS != RHS)
    Result = LIC->getInst(Inst::Ne, 1, {LHS, RHS});
  else        // (LHS == RHS)
    Result = LIC->getInst(Inst::Eq, 1, {LHS, RHS});

  if (Precondition)
    Ante = LIC->getInst(Inst::And, 1, {Ante, Precondition});
  // Result && RHS UB
  if (Mapping.RHS->K != Inst::Const)
    Result = LIC->getInst(Inst::And, 1, {Result, RHSUB});

  // (B)PCs && && LHS UB && (B)PCs UB
  Ante = LIC->getInst(Inst::And, 1, {Ante, LHSUB});

  // ((B)PCs && LHS UB && (B)PCs UB) => Result && RHS UB
  Result = getImpliesInst(Ante, Result);

  return Result;
}

std::string BuildQuery(InstContext &IC, const BlockPCs &BPCs,
    const std::vector<InstMapping> &PCs, InstMapping Mapping,
    std::vector<Inst *> *ModelVars, Inst *Precondition, bool Negate) {
  std::unique_ptr<ExprBuilder> EB;
  switch (SMTExprBuilder) {
  case ExprBuilder::KLEE:
    EB = createKLEEBuilder(IC);
    break;
  default:
    llvm::report_fatal_error("cannot reach here");
    break;
  }

  return EB->BuildQuery(BPCs, PCs, Mapping, ModelVars, Precondition, Negate);
}

Inst *getUBInstCondition(InstContext &IC, Inst *Root) {
  std::unique_ptr<ExprBuilder> EB;
  switch (SMTExprBuilder) {
  case ExprBuilder::KLEE:
    EB = createKLEEBuilder(IC);
    break;
  default:
    llvm::report_fatal_error("cannot reach here");
    break;
  }

  return EB->getUBInstCondition(Root);
}

}
