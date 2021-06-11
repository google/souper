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

#ifndef SOUPER_EXTRACTOR_EXPRBUILDER_H
#define SOUPER_EXTRACTOR_EXPRBUILDER_H

#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <unordered_map>

namespace souper {

class ExprBuilder {
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

  std::map<Block *, BlockPCPredMap> BlockPCMap;
  const unsigned MAX_PHI_DEPTH = 25;
public:
  enum Builder {
    KLEE
  };

  ExprBuilder(InstContext &IC) : LIC(&IC) {}
  virtual ~ExprBuilder() {};

  virtual std::string GetExprStr(const BlockPCs &BPCs,
                 const std::vector<InstMapping> &PCs, InstMapping Mapping,
                 std::vector<Inst *> *ModelVars, bool Negate=false,
                 bool DropUB = false) = 0;

  virtual std::string BuildQuery(const BlockPCs &BPCs,
                 const std::vector<InstMapping> &PCs, InstMapping Mapping,
                 std::vector<Inst *> *ModelVars, Inst *Precondition,  bool Negate=false,
                 bool DropUB = false) = 0;

  Inst *getDataflowConditions(Inst *I);
  Inst *getUBInstCondition(Inst *Root);
  Inst *GetCandidateExprForReplacement(
         const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
         InstMapping Mapping, Inst *Precondition, bool Negate, bool DropUB);
  void setBlockPCMap(const BlockPCs &BPCs);
  Inst *getBlockPCs(Inst *Root);
  std::vector<Inst *> getVarInsts(const std::vector<Inst *> Insts);
  Inst *getImpliesInst(Inst *Ante, Inst *I);
protected:
  InstContext *LIC;

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

  std::map<Inst *, Inst *> getUBInstConstraints(Inst *Root);
  std::vector<Inst *> getUBPathInsts(Inst *Root);

  Inst *getExtractInst(Inst *I, unsigned Offset, unsigned W);

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


};

std::string BuildQuery(InstContext &IC, const BlockPCs &BPCs,
       const std::vector<InstMapping> &PCs, InstMapping Mapping,
       std::vector<Inst *> *ModelVars, Inst *Precondition, bool Negate=false,
       bool DropUB=false);

std::unique_ptr<ExprBuilder> createKLEEBuilder(InstContext &IC);
Inst *getUBInstCondition(InstContext &IC, Inst *Root);
}

#endif  // SOUPER_EXTRACTOR_EXPRBUILDER_H
