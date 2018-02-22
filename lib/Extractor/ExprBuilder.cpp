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

#include "llvm/Support/CommandLine.h"
#include "souper/Extractor/ExprBuilder.h"

static llvm::cl::opt<bool> ExploitUB(
    "souper-exploit-ub",
    llvm::cl::desc("Exploit undefined behavior (default=true)"),
    llvm::cl::init(true));

using namespace llvm;
using namespace souper;

ExprBuilder::~ExprBuilder() {}

static llvm::cl::opt<souper::ExprBuilder::Builder> SMTExprBuilder(
    "souper-smt-expr-builder",
    llvm::cl::Hidden,
    llvm::cl::desc("SMT-LIBv2 expression builder (default=klee)"),
    llvm::cl::values(clEnumValN(souper::ExprBuilder::KLEE, "klee",
                                "Use KLEE's Expr library"),
                     clEnumValN(souper::ExprBuilder::Z3, "z3",
                                "Use Z3's API")),
    llvm::cl::init(souper::ExprBuilder::KLEE));

namespace souper {

std::vector<Inst *> ExprBuilder::getBlockPredicates(Inst *I) {
  assert(I->K == Inst::Phi && "not a phi inst");
  if (BlockPredMap.count(I->B))
    return BlockPredMap[I->B];
  std::vector<Inst *> PredExpr;
  for (auto const &PredVar : I->B->PredVars)
    PredExpr.push_back(PredVar);
  BlockPredMap[I->B] = PredExpr;
  return PredExpr;
}

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
      Paths.push_back(std::move(std::unique_ptr<UBPath>(New)));
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

  Inst *Pred = LIC->getConst(APInt(1, true));
  if (PathInst->K == Inst::Phi) {
    unsigned Num = BlockConstraints[PathInst->B];
    const auto &PredExpr = BlockPredMap[PathInst->B];
    // Sanity checks
    assert(PredExpr.size() && "there must be path predicates for the UBs");
    assert(PredExpr.size() == PathInst->Ops.size()-1 &&
           "phi predicate size mismatch");
    // Add the predicate(s)
    if (Num == 0)
      Pred = LIC->getInst(Inst::And, 1, {Pred, PredExpr[0]});
    else {
      Inst *IsZero = LIC->getInst(Inst::Eq, 1, {PredExpr[Num-1], LIC->getConst(APInt(PredExpr[Num-1]->Width, 0))});
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
      Inst *IsZero = LIC->getInst(Inst::Eq, 1, {SelectPred, LIC->getConst(APInt(SelectPred->Width, 0))});
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
// (2) then for each path, we generate a corresponding KLEE expression
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
// (1) caching the KLEE expresions for each processed phi, where each
//     KLEE expression encodes the path that starts from one of the phi's
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
//     we first encounter phi %11. The generated KLEE expression
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
Inst *ExprBuilder::getUBInstCondition() {
#if 0
  // A map from a Phi instruction to all of its KLEE expressions that
  // encode the path and UB Inst predicates.
  UBPathInstMap CachedUBPathInsts;
  std::set<Inst *> UsedUBInsts;
  Inst *Result = LIC->getConst(APInt(1, true));
  // For each Phi/Select instruction
  for (const auto &I : UBPathInsts) {
    if (CachedUBPathInsts.count(I) != 0)
      continue;
    // Recursively collect UB instructions
    // on the block constrained Phi and Select branches
    std::vector<std::unique_ptr<UBPath>> UBPaths;
    UBPath *Current = new UBPath;
    UBPaths.push_back(std::move(std::unique_ptr<UBPath>(Current)));
    if (!getUBPaths(I, Current, UBPaths, CachedUBPathInsts, 0))
      return LIC->getConst(APInt(1, true));
    CachedUBPathInsts[I] = {};
    // For each found path
    for (const auto &Path : UBPaths) {
      if (!Path->UBInsts.size())
        continue;
      // Aggregate collected UB constraints
      Inst *Ante = LIC->getConst(APInt(1, true));
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
      Inst *IsZero = IC.getInst(Inst::Eq, 1, {Pred, LIC->getConst(APInt(1, false))});
      Inst *Implies = IC.getInst(Inst::Or, 1, {IsZero, Ante});
      Result = LIC->getInst(Inst::And, 1, {Result, Implies});
    }
  }
  // Add the unconditional UB constraints at the top level
  for (const auto &Entry: UBExprMap)
    if (!UsedUBInsts.count(Entry.first))
      Result = LIC->getInst(Inst::And, 1, {Result, Entry.second});

  return Result;
#endif
  return LIC->getConst(APInt(1, true));
}

// Similar to the way we collect UB constraints. We could combine it with 
// getUBInstCondition, because the workflow is quite similar. 
// However, mixing two parts (one for UB constraints, one for BlockPCs)
// may make the code less structured. If we see big performance overhead,
// we may consider to combine these two parts together. 
Inst *ExprBuilder::getBlockPCs() {

  UBPathInstMap CachedPhis;
  Inst *Result = LIC->getConst(APInt(1, true));
  // For each Phi instruction
  for (const auto &I : UBPathInsts) {
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
      Inst *Ante = LIC->getConst(APInt(1, true));
      for (const auto &PC : Path->PCs)
        Ante = LIC->getInst(Inst::And, 1, {Ante, PC});
      // Create path predicate
      Inst *Pred =
        createUBPathInstsPred(I, Path->Phis, Path->BlockConstraints,
                              /*SelectBranches=*/nullptr, CachedPhis);
      // Add predicate->UB constraint
      Inst *IsZero = LIC->getInst(Inst::Eq, 1, {Pred, LIC->getConst(APInt(1, false))});
      Inst *Implies = LIC->getInst(Inst::Or, 1, {IsZero, Ante});
      Result = LIC->getInst(Inst::And, 1, {Result, Implies});
    }
  }

  return Result;
}

void ExprBuilder::setBlockPCMap(const BlockPCs &BPCs) {
  for (auto BPC : BPCs) {
    assert(BPC.B && "Block is NULL!");
    BlockPCPredMap &PCMap = BlockPCMap[BPC.B];
    auto I = PCMap.find(BPC.PredIdx);
    // Relying on a class-level flag may not be a nice solution,
    // but it seems hard to differentiate two cases:
    //   (1) UBInstExpr collected through blockpc, and;
    //   (2) UBInstExpr collected through pc/lhs/rhs
    // For the first case, UBInst(s) is conditional, i.e.,
    // they rely on the fact that blockpc(s) are true.
    if (I != PCMap.end())
      UBInstPrecondition = I->second;
    IsForBlockPCUBInst = true;
    Inst *PE = getInstMapping(BPC.PC);
    IsForBlockPCUBInst = false;
    UBInstPrecondition = nullptr;
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
  Inst *Pred = LIC->getConst(APInt(1, true));
  for (const auto &PathInst : PathInsts) {
    if (PathInst->Ops.size() == 1)
      continue;
    Inst *InstPred = createPathPred(BlockConstraints, PathInst, SelectBranches);

    UBPathInstMap::iterator PI = CachedUBPathInsts.find(PathInst);
    if (PI == CachedUBPathInsts.end()) {
       // It's possible that we don't have a cached instruction yet,
       // e.g., the CurrentInst is a select operator.
       assert(CurrentInst->K == Inst::Select && "No cached Inst?");
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

void ExprBuilder::recordUBInstruction(Inst *I, Inst *E) {
#if 0
  if (!IsForBlockPCUBInst) {
    UBExprMap[I] = E;
  }
  else if (!UBInstPrecondition.isNull()) {
    // The current UBInst comes from BlockPC. It's possible
    // that the precondition is missing at this point (e.g.,
    // the corresponding Phi is not part of the current
    // Souper IR because the Phi is not in the equivalence class
    // of the instruction.
    Inst *IsZero = LIC->getInst(Inst::Eq, 1, {UBInstPrecondition, LIC->getConst(APInt(1, false))});
    UBExprMap[I] = LIC->getInst(Inst::Or, 1, {IsZero, E});
  }
#endif
}
  
Inst *ExprBuilder::getZeroBitsMapping(Inst *I) {
  return ZeroBitsMap[I];
}

Inst *ExprBuilder::getOneBitsMapping(Inst *I) {
  return OneBitsMap[I];
}

Inst *ExprBuilder::getNonZeroBitsMapping(Inst *I) {
  return NonZeroBitsMap[I];
}

Inst *ExprBuilder::getNonNegBitsMapping(Inst *I) {
  return NonNegBitsMap[I];
}

Inst *ExprBuilder::getNegBitsMapping(Inst *I) {
  return NegBitsMap[I];
}

Inst *ExprBuilder::getPowerTwoBitsMapping(Inst *I) {
  return PowerTwoBitsMap[I];
}

Inst *ExprBuilder::getSignBitsMapping(Inst *I) {
  return SignBitsMap[I];
}

Inst *ExprBuilder::getInstMapping(const InstMapping &IM) {
  return LIC->getInst(Inst::Eq, 1, {IM.LHS, IM.RHS});
}

// Return an expression which must be proven valid for the candidate to apply.
llvm::Optional<ExprBuilder::CandidateExpr> ExprBuilder::GetCandidateExprForReplacement(
    const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
    InstMapping Mapping, bool Negate) {
  // Build LHS
  Inst *LHS = Mapping.LHS;
  Inst *Ante = LIC->getConst(APInt(1, true));
  llvm::APInt DemandedBits = Mapping.LHS->DemandedBits;
  if (!DemandedBits.isAllOnesValue())
    LHS = LIC->getInst(Inst::And, LHS->Width,
                       {LHS, LIC->getConst(DemandedBits)});
  for (const auto I : CE.Vars) {
    if (I) {
      if (I->KnownZeros.getBoolValue() || I->KnownOnes.getBoolValue()) {
        Ante = LIC->getInst(Inst::And, 1, {Ante, getZeroBitsMapping(I)});
        Ante = LIC->getInst(Inst::And, 1, {Ante, getOneBitsMapping(I)});
      }
      if (I->NonZero)
        Ante = LIC->getInst(Inst::And, 1, {Ante, getNonZeroBitsMapping(I)});
      if (I->NonNegative)
        Ante = LIC->getInst(Inst::And, 1, {Ante, getNonNegBitsMapping(I)});
      if (I->PowOfTwo)
        Ante = LIC->getInst(Inst::And, 1, {Ante, getPowerTwoBitsMapping(I)});
      if (I->Negative)
        Ante = LIC->getInst(Inst::And, 1, {Ante, getNegBitsMapping(I)});
      if (I->NumSignBits > 1)
        Ante = LIC->getInst(Inst::And, 1, {Ante, getSignBitsMapping(I)});
    }
  }
  // Build PCs
  for (const auto &PC : PCs)
    Ante = LIC->getInst(Inst::And, 1, {Ante, getInstMapping(PC)});
  // Build BPCs 
  if (BPCs.size()) {
    setBlockPCMap(BPCs);
    Ante = LIC->getInst(Inst::And, 1, {Ante, getBlockPCs()});
  }
  // Get UB constraints of LHS and (B)PCs
  Inst *LHSPCsUB = LIC->getConst(APInt(1, true));
  if (ExploitUB) {
    LHSPCsUB = getUBInstCondition();
    //TODO
    //if (LHSPCsUB.isNull())
    //  return llvm::Optional<CandidateExpr>();
  }
  // Build RHS
  Inst *RHS = Mapping.RHS;
  if (!DemandedBits.isAllOnesValue())
    RHS = LIC->getInst(Inst::And, RHS->Width,
                       {RHS, LIC->getConst(DemandedBits)});
  // Get all UB constraints (LHS && (B)PCs && RHS)
  Inst *UB = LIC->getConst(APInt(1, true));
  if (ExploitUB) {
    UB = getUBInstCondition();
    //TODO
    //if (UB.isNull())
    //  return llvm::Optional<CandidateExpr>();
  }

  Inst *Cons;
  if (Negate) // (LHS != RHS)
    Cons = LIC->getInst(Inst::Ne, 1, {LHS, RHS});
  else        // (LHS == RHS)
    Cons = LIC->getInst(Inst::Eq, 1, {LHS, RHS});
  // Cons && UB
  if (Mapping.RHS->K != Inst::Const)
    Cons = LIC->getInst(Inst::And, Cons->Width, {Cons, UB});
  // (LHS UB && (B)PCs && (B)PCs UB)
  Ante = LIC->getInst(Inst::And, 1, {Ante, LHSPCsUB});
  // (LHS UB && (B)PCs && (B)PCs UB) => Cons && UB
  Inst *IsZero = LIC->getInst(Inst::Eq, 1,
                              {Ante, LIC->getConst(APInt(1, false))});
  CE.E = LIC->getInst(Inst::Or, 1, {IsZero, Cons});

  return llvm::Optional<CandidateExpr>(std::move(CE));
}

std::string BuildQuery(InstContext &IC, const BlockPCs &BPCs,
       const std::vector<InstMapping> &PCs, InstMapping Mapping,
       std::vector<Inst *> *ModelVars, bool Negate) {
  std::unique_ptr<ExprBuilder> EB;
  switch (SMTExprBuilder) {
  case ExprBuilder::KLEE:
    EB = createKLEEBuilder(IC);
    break;
  case ExprBuilder::Z3:
    report_fatal_error("not supported yet");
    break;
  default:
    report_fatal_error("cannot reach here");
    break;
  }

  return EB->BuildQuery(BPCs, PCs, Mapping, ModelVars, Negate);
}

}
