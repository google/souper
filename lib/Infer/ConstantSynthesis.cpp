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
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/Pruning.h"

extern unsigned DebugLevel;

namespace {
  using namespace llvm;
  static cl::opt<unsigned> MaxSpecializations("souper-constant-synthesis-max-num-specializations",
    cl::desc("Maximum number of input specializations in constant synthesis (default=15)."),
    cl::init(15));
}
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

  if (Pruner) {
    size_t Specializations = 0;
    for (auto &&VC : Pruner->getInputVals()) {
      if (Specializations++ >= MaxSpecializations) {
        break;
      }
      std::map<Inst *, llvm::APInt> VCCopy;
      for (auto Pair : VC) {
        if (Pair.second.hasValue()) {
          VCCopy[Pair.first] = Pair.second.getValue();
        }
      }
      if (!VCCopy.empty()) {
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        TriedAnte = IC.getInst(Inst::And, 1,
                    {IC.getInst(Inst::Eq, 1,
                    {getInstCopy(Mapping.LHS, IC, InstCache, BlockCache, &VCCopy, true),
                    getInstCopy(Mapping.RHS, IC, InstCache, BlockCache, &VCCopy, true)}), TriedAnte});
      }
    }
  }

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
    std::vector<Block *> Blocks = getBlocksFromPhis(LHSCopy);
    for (auto Block : Blocks) {
      Block->ConcretePred = 0;
    }


    if (DebugLevel > 2) {
      if (Pruner) {
        if (Pruner->isInfeasible(RHSCopy, DebugLevel)) {
          //TODO(manasij)
          llvm::errs() << "Second Query Skipping opportunity.\n";
        }
      }
    }

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
        llvm::errs() << I << " th attempt: " << "second query is SAT-- constant doesn't work\n";
      }

      std::map<Inst *, llvm::APInt> SubstConstMap;
      ValueCache VC;
      for (unsigned J = 0; J != ModelInstsSecondQuery.size(); ++J) {
        Inst* Var = ModelInstsSecondQuery[J];
        if (Var->Name == BlockPred && !ModelValsSecondQuery[J].isNullValue())
          for (auto B : Blocks)
            for (unsigned I = 0 ; I < B->PredVars.size(); ++I)
              if (B->PredVars[I] == Var)
                B->ConcretePred = I + 1;

        if (ConstSet.find(Var) == ConstSet.end()) {
          SubstConstMap.insert(std::pair<Inst *, llvm::APInt>(Var, ModelValsSecondQuery[J]));
          VC.insert(std::pair<Inst *, llvm::APInt>(Var, ModelValsSecondQuery[J]));
        }
      }

      ConcreteInterpreter CI(LHSCopy, VC);
      auto LHSV = CI.evaluateInst(LHSCopy);

      if (!LHSV.hasValue()) {
        llvm::report_fatal_error("the model returned from second query evaluates to poison for LHS");
      }

      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      SubstAnte = IC.getInst(Inst::And, 1,
                             {IC.getInst(Inst::Eq, 1, {IC.getConst(LHSV.getValue()),
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
