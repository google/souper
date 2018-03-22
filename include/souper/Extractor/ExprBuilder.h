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

#include "klee/Expr.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <unordered_map>

using namespace klee;
using namespace souper;

namespace souper {

class ExprBuilder {
public:
  enum Builder {
    KLEE,
    Z3
  };

  // Local reference
  InstContext *LIC;

  const unsigned MAX_PHI_DEPTH = 25;
  
  typedef std::unordered_map<Inst *, std::vector<Inst *>> UBPathInstMap;
  typedef std::map<unsigned, Inst *> BlockPCPredMap;
  
  struct UBPath {
    std::map<Block *, unsigned> BlockConstraints;
    std::map<Inst *, bool> SelectBranches;
    std::vector<Inst *> Insts;
    std::vector<Inst *> UBInsts;
  };
  
  struct BlockPCPhiPath {
    std::map<Block *, unsigned> BlockConstraints;
    std::vector<Inst *> Phis;
    std::vector<Inst *> PCs;
  };

  virtual ~ExprBuilder();

  std::vector<Inst *> getBlockPredicates(Inst *I);
  bool getUBPaths(Inst *I, UBPath *Current,
                  std::vector<std::unique_ptr<UBPath>> &Paths,
                  UBPathInstMap &CachedUBPathInsts, unsigned Depth);
  void getBlockPCPhiPaths(Inst *I, BlockPCPhiPath *Current,
                          std::vector<std::unique_ptr<BlockPCPhiPath>> &Paths,
                          UBPathInstMap &CachedPhis);
  Inst *createPathPred(std::map<Block *, unsigned> &BlockConstraints,
                       Inst* PathInst,
                       std::map<Inst *, bool> *SelectBranches);
  Inst *createUBPathInstsPred(Inst *CurrentInst,
                              std::vector<Inst *> &UBPathInsts,
                              std::map<Block *, unsigned> &BlockConstraints,
                              std::map<Inst *, bool> *SelectBranches,
                              UBPathInstMap &CachedUBPathInsts);

  Inst *getInstMapping(const InstMapping &IM);
  Inst *getUBInstCondition();
  Inst *getDemandedBitsCondition(Inst *I);
  Inst *getBlockPCs();
  void setBlockPCMap(const BlockPCs &BPCs);
  void recordUBInstruction(Inst *I, Inst *E);

  std::map<Block *, std::vector<Inst *>> BlockPredMap;

  std::map<Inst *, Inst *> UBExprMap;

  std::map<Block *, BlockPCPredMap> BlockPCMap;
  std::vector<Inst *> UBPathInsts;
  UniqueNameSet ArrayNames;
  // Holding the precondition, i.e. blockpc, for the UBInst under process.
  Inst *UBInstPrecondition = nullptr;
  // Indicate if the UBInst relates to BlockPC
  bool IsForBlockPCUBInst = false;

  struct CandidateExpr {
    std::vector<std::unique_ptr<Array>> Arrays;
    std::vector<Inst *> Vars;
    ref<Expr> E;
  };
  CandidateExpr CE;

  Inst *getExtractInst(Inst *I, unsigned Offset, unsigned W);
  Inst *getImpliesInst(Inst *Ante, Inst *I);

  Inst *addnswUB(Inst *I);
  Inst *addnuwUB(Inst *I);
  Inst *subnswUB(Inst *I);
  Inst *subnuwUB(Inst *I);
  Inst *mulnswUB(Inst *I);
  Inst *mulnuwUB(Inst *I);
  Inst *udivUB(Inst *I);
  Inst *udivExactUB(Inst *I);
  Inst *sdivUB(Inst *I);
  Inst *sdivExactUB(Inst *I);
  Inst *shiftUB(Inst *I);
  Inst *shlnswUB(Inst *I);
  Inst *shlnuwUB(Inst *I);
  Inst *lshrExactUB(Inst *I);
  Inst *ashrExactUB(Inst *I);

  virtual llvm::Optional<CandidateExpr> GetCandidateExprForReplacement(
      const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
      InstMapping Mapping, bool Negate) = 0;
  
  virtual std::string BuildQuery(const BlockPCs &BPCs,
                 const std::vector<InstMapping> &PCs, InstMapping Mapping,
                 std::vector<Inst *> *ModelVars, bool Negate=false) = 0;
};

std::string BuildQuery(InstContext &IC, const BlockPCs &BPCs,
       const std::vector<InstMapping> &PCs, InstMapping Mapping,
       std::vector<Inst *> *ModelVars, bool Negate=false);

std::unique_ptr<ExprBuilder> createKLEEBuilder(InstContext &IC);

}

#endif  // SOUPER_EXTRACTOR_EXPRBUILDER_H
