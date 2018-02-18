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

#ifndef SOUPER_EXTRACTOR_EXPRBUILDER_H
#define SOUPER_EXTRACTOR_EXPRBUILDER_H

#include "souper/Extractor/Candidates.h"
#include "souper/Util/UniqueNameSet.h"
#include <unordered_map>

namespace souper {

typedef void *SMTExpr;
typedef void *Array;

struct CandidateExpr {
  //std::vector<std::unique_ptr<klee::Array>> Arrays;
  //std::vector<Inst *> ArrayVars;
  //klee::ref<klee::Expr> E;
  SMTExpr E;
};

const unsigned MAX_PHI_DEPTH = 25;

typedef std::unordered_map<Inst *, std::vector<SMTExpr>> UBPathInstMap;
typedef std::map<unsigned, SMTExpr> BlockPCPredMap;

struct UBPath {
  std::map<Block *, unsigned> BlockConstraints;
  std::map<Inst *, bool> SelectBranches;
  std::vector<Inst *> Insts;
  std::vector<Inst *> UBInsts;
};

struct BlockPCPhiPath {
  std::map<Block *, unsigned> BlockConstraints;
  std::vector<Inst *> Phis;
  std::vector<SMTExpr> PCs;
};

class ExprBuilder {
public:
  ExprBuilder(std::vector<std::unique_ptr<Array>> &Arrays,
              std::vector<Inst *> &ArrayVars);
  virtual ~ExprBuilder();

  virtual SMTExpr addnswUB(Inst *I) = 0;
  virtual SMTExpr addnuwUB(Inst *I) = 0;
  virtual SMTExpr subnswUB(Inst *I) = 0;
  virtual SMTExpr subnuwUB(Inst *I) = 0;
  virtual SMTExpr mulnswUB(Inst *I) = 0;
  virtual SMTExpr mulnuwUB(Inst *I) = 0;
  virtual SMTExpr udivUB(Inst *I) = 0;
  virtual SMTExpr udivExactUB(Inst *I) = 0;
  virtual SMTExpr sdivUB(Inst *I) = 0;
  virtual SMTExpr sdivExactUB(Inst *I) = 0;
  virtual SMTExpr shiftUB(Inst *I) = 0;
  virtual SMTExpr shlnswUB(Inst *I) = 0;
  virtual SMTExpr shlnuwUB(Inst *I) = 0;
  virtual SMTExpr lshrExactUB(Inst *I) = 0;
  virtual SMTExpr ashrExactUB(Inst *I) = 0;
  virtual SMTExpr countOnes(SMTExpr E) = 0;

  virtual void recordUBInstruction(Inst *I, SMTExpr E) = 0;
  SMTExpr buildAssoc(std::function<SMTExpr(SMTExpr, SMTExpr)> F,
                       llvm::ArrayRef<Inst *> Ops);
  virtual SMTExpr build(Inst *I) = 0;
  virtual SMTExpr get(Inst *I) = 0;
  virtual SMTExpr getInstMapping(const InstMapping &IM) = 0;
  SMTExpr getZeroBitsMapping(Inst *I);
  SMTExpr getOneBitsMapping(Inst *I);
  SMTExpr getNonZeroBitsMapping(Inst *I);
  SMTExpr getNonNegBitsMapping(Inst *I);
  SMTExpr getPowerTwoBitsMapping(Inst *I);
  SMTExpr getNegBitsMapping(Inst *I);
  SMTExpr getSignBitsMapping(Inst *I);
  std::vector<SMTExpr> getBlockPredicates(Inst *I);
  virtual SMTExpr getUBInstCondition() = 0;
  virtual SMTExpr getBlockPCs() = 0;
  virtual void setBlockPCMap(const BlockPCs &BPCs) = 0;
  virtual SMTExpr createPathPred(std::map<Block *, unsigned> &BlockConstraints,
                           Inst* PathInst,
                           std::map<Inst *, bool> *SelectBranches) = 0;
  virtual SMTExpr createUBPathInstsPred(Inst *CurrentInst,
                           std::vector<Inst *> &UBPathInsts,
                           std::map<Block *, unsigned> &BlockConstraints,
                           std::map<Inst *, bool> *SelectBranches,
                           UBPathInstMap &CachedUBPathInsts) = 0;
  bool getUBPaths(Inst *I, UBPath *Current,
                  std::vector<std::unique_ptr<UBPath>> &Paths,
                  UBPathInstMap &CachedUBPathInsts, unsigned Depth);
  void getBlockPCPhiPaths(Inst *I, BlockPCPhiPath *Current,
                          std::vector<std::unique_ptr<BlockPCPhiPath>> &Paths,
                          UBPathInstMap &CachedPhis);

  std::map<Block *, std::vector<SMTExpr>> BlockPredMap;
  std::map<Inst *, SMTExpr> ExprMap;
  std::map<Inst *, SMTExpr> UBExprMap;
  std::map<Inst *, SMTExpr> ZeroBitsMap;
  std::map<Inst *, SMTExpr> OneBitsMap;
  std::map<Inst *, SMTExpr> NonZeroBitsMap;
  std::map<Inst *, SMTExpr> NonNegBitsMap;
  std::map<Inst *, SMTExpr> PowerTwoBitsMap;
  std::map<Inst *, SMTExpr> NegBitsMap;
  std::map<Inst *, SMTExpr> SignBitsMap;
  std::map<Block *, BlockPCPredMap> BlockPCMap;
  std::vector<std::unique_ptr<Array>> &Arrays;
  std::vector<Inst *> &ArrayVars;
  std::vector<Inst *> UBPathInsts;
  UniqueNameSet ArrayNames;
  // Holding the precondition, i.e. blockpc, for the UBInst under process.
  SMTExpr UBInstPrecondition;
  // Indicate if the UBInst relates to BlockPC
  bool IsForBlockPCUBInst;

};

llvm::Optional<CandidateExpr> GetCandidateExprForReplacement(
    const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
    InstMapping Mapping, bool Negate);

std::string BuildQuery(const BlockPCs &BPCs,
               const std::vector<InstMapping> &PCs, InstMapping Mapping,
               std::vector<Inst *> *ModelVars, bool Negate=false);

}

#endif  // SOUPER_EXTRACTOR_EXPRBUILDER_H
