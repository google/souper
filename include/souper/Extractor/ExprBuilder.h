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

  ref<Expr> buildAssoc(std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
                       llvm::ArrayRef<Inst *> Ops);

  ref<Expr> getZeroBitsMapping(Inst *I);
  ref<Expr> getOneBitsMapping(Inst *I);
  ref<Expr> getNonZeroBitsMapping(Inst *I);
  ref<Expr> getNonNegBitsMapping(Inst *I);
  ref<Expr> getPowerTwoBitsMapping(Inst *I);
  ref<Expr> getNegBitsMapping(Inst *I);
  ref<Expr> getSignBitsMapping(Inst *I);

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
  Inst *getBlockPCs();
  void setBlockPCMap(const BlockPCs &BPCs);
  void recordUBInstruction(Inst *I, Inst *E);

  std::map<Block *, std::vector<Inst *>> BlockPredMap;

  std::map<Inst *, ref<Expr>> ExprMap;
  std::map<Inst *, Inst *> UBExprMap;

  std::map<Inst *, ref<Expr>> ZeroBitsMap;
  std::map<Inst *, ref<Expr>> OneBitsMap;
  std::map<Inst *, ref<Expr>> NonZeroBitsMap;
  std::map<Inst *, ref<Expr>> NonNegBitsMap;
  std::map<Inst *, ref<Expr>> PowerTwoBitsMap;
  std::map<Inst *, ref<Expr>> NegBitsMap;
  std::map<Inst *, ref<Expr>> SignBitsMap;

  std::map<Block *, BlockPCPredMap> BlockPCMap;
  std::vector<Inst *> UBPathInsts;
  UniqueNameSet ArrayNames;
  // Holding the precondition, i.e. blockpc, for the UBInst under process.
  Inst *UBInstPrecondition;
  // Indicate if the UBInst relates to BlockPC
  bool IsForBlockPCUBInst = false;

  struct CandidateExpr {
    std::vector<std::unique_ptr<Array>> Arrays;
    std::vector<Inst *> ArrayVars;
    ref<Expr> E;
  };
  CandidateExpr CE;
  virtual llvm::Optional<CandidateExpr> GetCandidateExprForReplacement(
      const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
      InstMapping Mapping, bool Negate) = 0;
  
  virtual std::string BuildQuery(const BlockPCs &BPCs,
                 const std::vector<InstMapping> &PCs, InstMapping Mapping,
                 std::vector<Inst *> *ModelVars, bool Negate=false) = 0;

protected:
  virtual ref<Expr> addnswUB(Inst *I) = 0;
  virtual ref<Expr> addnuwUB(Inst *I) = 0;
  virtual ref<Expr> subnswUB(Inst *I) = 0;
  virtual ref<Expr> subnuwUB(Inst *I) = 0;
  virtual ref<Expr> mulnswUB(Inst *I) = 0;
  virtual ref<Expr> mulnuwUB(Inst *I) = 0;
  virtual ref<Expr> udivUB(Inst *I) = 0;
  virtual ref<Expr> udivExactUB(Inst *I) = 0;
  virtual ref<Expr> sdivUB(Inst *I) = 0;
  virtual ref<Expr> sdivExactUB(Inst *I) = 0;
  virtual ref<Expr> shiftUB(Inst *I) = 0;
  virtual ref<Expr> shlnswUB(Inst *I) = 0;
  virtual ref<Expr> shlnuwUB(Inst *I) = 0;
  virtual ref<Expr> lshrExactUB(Inst *I) = 0;
  virtual ref<Expr> ashrExactUB(Inst *I) = 0;
  virtual ref<Expr> countOnes(ref<Expr> E) = 0;

  virtual ref<Expr> build(Inst *I) = 0;
  virtual ref<Expr> get(Inst *I) = 0;

};

std::string BuildQuery(InstContext &IC, const BlockPCs &BPCs,
       const std::vector<InstMapping> &PCs, InstMapping Mapping,
       std::vector<Inst *> *ModelVars, bool Negate=false);

std::unique_ptr<ExprBuilder> createKLEEBuilder(InstContext &IC);

}

#endif  // SOUPER_EXTRACTOR_EXPRBUILDER_H
