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

ref<Expr> ExprBuilder::buildAssoc(
    std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
    llvm::ArrayRef<Inst *> Ops) {
  ref<Expr> E = get(Ops[0]);
  for (Inst *I : llvm::ArrayRef<Inst *>(Ops.data()+1, Ops.size()-1)) {
    E = F(E, get(I));
  }
  return E;
}

std::vector<ref<Expr>> ExprBuilder::getBlockPredicates(Inst *I) {
  assert(I->K == Inst::Phi && "not a phi inst");
  if (BlockPredMap.count(I->B))
    return BlockPredMap[I->B];
  std::vector<ref<Expr>> PredExpr;
  for (auto const &PredVar : I->B->PredVars)
    PredExpr.push_back(build(PredVar));
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

ref<Expr> ExprBuilder::getZeroBitsMapping(Inst *I) {
  return ZeroBitsMap[I];
}

ref<Expr> ExprBuilder::getOneBitsMapping(Inst *I) {
  return OneBitsMap[I];
}

ref<Expr> ExprBuilder::getNonZeroBitsMapping(Inst *I) {
  return NonZeroBitsMap[I];
}

ref<Expr> ExprBuilder::getNonNegBitsMapping(Inst *I) {
  return NonNegBitsMap[I];
}

ref<Expr> ExprBuilder::getNegBitsMapping(Inst *I) {
  return NegBitsMap[I];
}

ref<Expr> ExprBuilder::getPowerTwoBitsMapping(Inst *I) {
  return PowerTwoBitsMap[I];
}

ref<Expr> ExprBuilder::getSignBitsMapping(Inst *I) {
  return SignBitsMap[I];
}

// TODO: Fix GetCandidateExprForReplacement

std::string BuildQuery(const BlockPCs &BPCs,
       const std::vector<InstMapping> &PCs, InstMapping Mapping,
       std::vector<Inst *> *ModelVars, bool Negate) {
  std::unique_ptr<ExprBuilder> EB;
  switch (SMTExprBuilder) {
  case ExprBuilder::KLEE:
    EB = createKLEEBuilder();
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
