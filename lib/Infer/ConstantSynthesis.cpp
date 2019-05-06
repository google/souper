// Copyright 2019 The Souper Authors. All rights reserved.
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

#define DEBUG_TYPE "souper"

#include "llvm/ADT/APInt.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Infer/ConstantSynthesis.h"

extern unsigned DebugLevel;

namespace souper {

std::error_code
ConstantSynthesis::synthesize(SMTLIBSolver *SMTSolver,
                              const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              InstMapping Mapping, std::set<Inst *> &ConstSet,
                              std::map <Inst *, llvm::APInt> &ResultMap,
                              InstContext &IC, unsigned MaxTries, unsigned Timeout) {

  Inst *TrueConst = IC.getConst(llvm::APInt(1, true));
  Inst *FalseConst = IC.getConst(llvm::APInt(1, false));


  // generalization by substitution
  Inst *SubstAnte = TrueConst;
  Inst *TriedAnte = TrueConst;
  std::error_code EC;

  for (int I = 0 ; I < MaxTries; I ++)  {
    bool IsSat;
    std::vector<Inst *> ModelInstsFirstQuery;
    std::vector<llvm::APInt> ModelValsFirstQuery;

    // TriedAnte /\ SubstAnte
    Inst *FirstQueryAnte = IC.getInst(Inst::And, 1, {SubstAnte, TriedAnte});

    std::string Query = BuildQuery(IC, BPCs, PCs, InstMapping(Mapping.LHS, Mapping.RHS),
                                   &ModelInstsFirstQuery, FirstQueryAnte, true);


    if (Query.empty())
      return std::make_error_code(std::errc::value_too_large);

    EC = SMTSolver->isSatisfiable(Query, IsSat, ModelInstsFirstQuery.size(),
                                  &ModelValsFirstQuery, Timeout);

    if (EC) {
      if (DebugLevel > 3) {
        llvm::errs()<<"ConstantSynthesis: solver returns error on first query\n";
      }
      return EC;
    }

    if (!IsSat) {
      // no constant found
      if (DebugLevel > 3) {
        llvm::errs() << "first query is UNSAT-- no more guesses\n";
      }
      return std::error_code();
    }

    if (DebugLevel > 3) {
      llvm::errs() << "first query is SAT, returning the model:\n";
    }

    Inst* TriedAnteLocal = FalseConst;
    std::map<Inst *, llvm::APInt> ConstMap;
    for (unsigned J = 0; J != ModelInstsFirstQuery.size(); ++J) {
      if (ConstSet.find(ModelInstsFirstQuery[J]) != ConstSet.end()) {
        if (DebugLevel > 3) {
          llvm::errs() << ModelInstsFirstQuery[J]->Name;
          llvm::errs() << ": ";
          llvm::errs() << ModelValsFirstQuery[J];
          llvm::errs() << "\n";
        }

        Inst *Const = IC.getConst(ModelValsFirstQuery[J]);
        ConstMap.insert(std::pair<Inst *, llvm::APInt>(ModelInstsFirstQuery[J], Const->Val));
        Inst *Ne = IC.getInst(Inst::Ne, 1, {ModelInstsFirstQuery[J], Const});
        if (ConstSet.size() == 1) {
          TriedAnteLocal = Ne;
        } else {
          TriedAnteLocal = IC.getInst(Inst::Or, 1, {TriedAnteLocal, Ne});
        }
      }
    }
    TriedAnte = IC.getInst(Inst::And, 1, {TriedAnte, TriedAnteLocal});


    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    Inst *LHSCopy = getInstCopy(Mapping.LHS, IC, InstCache, BlockCache, &ConstMap, false);
    Inst *RHSCopy = getInstCopy(Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, false);

    BlockPCs BPCsCopy;
    std::vector<InstMapping> PCsCopy;
    separateBlockPCs(BPCs, BPCsCopy, InstCache, BlockCache, IC, &ConstMap, false);
    separatePCs(PCs, PCsCopy, InstCache, BlockCache, IC, &ConstMap, false);

    // check if the constant is valid for all inputs
    std::vector<Inst *> ModelInstsSecondQuery;
    std::vector<llvm::APInt> ModelValsSecondQuery;

    Query = BuildQuery(IC, BPCsCopy, PCsCopy, InstMapping(LHSCopy, RHSCopy),
                       &ModelInstsSecondQuery, 0);

    if (Query.empty())
      return std::make_error_code(std::errc::value_too_large);

    EC = SMTSolver->isSatisfiable(Query, IsSat, ModelInstsSecondQuery.size(),
                                  &ModelValsSecondQuery, Timeout);
    if (EC) {
      if (DebugLevel > 3) {
        llvm::errs()<<"ConstantSynthesis: solver returns error on second query\n";
      }
      return EC;
    }

    if (!IsSat) {
      if (DebugLevel > 3) {
        llvm::errs() << "second query is UNSAT-- this guess works\n";
      }
      ResultMap = std::move(ConstMap);
      return EC;
    } else {
      if (DebugLevel > 3) {
        llvm::errs() << "second query is SAT-- constant doesn't work\n";
      }

      std::map<Inst *, llvm::APInt> SubstConstMap;
      for (unsigned J = 0; J != ModelInstsSecondQuery.size(); ++J) {
        Inst* Var = ModelInstsSecondQuery[J];

        if (ConstSet.find(Var) == ConstSet.end()) {
          SubstConstMap.insert(std::pair<Inst *, llvm::APInt>(Var, ModelValsSecondQuery[J]));
        }
      }
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      SubstAnte = IC.getInst(Inst::And, 1,
                             {IC.getInst(Inst::Eq, 1, {getInstCopy(Mapping.LHS, IC, InstCache,
                                                                   BlockCache, &SubstConstMap, true),
                                                       getInstCopy(Mapping.RHS, IC, InstCache,
                                                                   BlockCache, &SubstConstMap, true)}),
                              SubstAnte});
    }
  }

  if (DebugLevel > 3) {
    llvm::errs() << "number of constant synthesis tries exceeds MaxTries(";
    llvm::errs() << MaxTries;
    llvm::errs() << ")\n";
  }
  return EC;
}

}
