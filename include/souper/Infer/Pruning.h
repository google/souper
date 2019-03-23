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

#ifndef SOUPER_PRUNING_H
#define SOUPER_PRUNING_H

#include "llvm/ADT/APInt.h"

#include "souper/Extractor/Solver.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Inst/Inst.h"

#include <unordered_map>

namespace souper {

typedef std::function<bool(Inst *, std::vector<Inst *>&)> PruneFunc;

class PruningManager {
public:
  PruningManager(SynthesisContext &SC_, std::vector< souper::Inst *> &Inputs_,
                 unsigned int StatsLevel_);
  PruneFunc getPruneFunc() {return DataflowPrune;}
  void printStats(llvm::raw_ostream &out) {
    out << "Dataflow Pruned " << NumPruned << "/" << TotalGuesses << "\n";
  }

  bool isInfeasible(Inst *RHS, unsigned StatsLevel);
  bool isInfeasibleWithSolver(Inst *RHS, unsigned StatsLevel);
  void init();
  // double init antipattern, required because init should
  // not be called when pruning is disabled
private:
  SynthesisContext &SC;
  std::vector<ConcreteInterpreter> ConcreteInterpreters;
  std::vector<llvm::KnownBits> LHSKnownBits;
  std::vector<llvm::ConstantRange> LHSConstantRange;
  bool LHSHasPhi = false;

  PruneFunc DataflowPrune;
  unsigned NumPruned;
  unsigned TotalGuesses;
  int StatsLevel;
  std::vector<ValueCache> InputVals;
  std::vector<Inst *> &InputVars;
  std::vector<ValueCache> generateInputSets(std::vector<Inst *> &Inputs);
  // For the LHS contained in @SC, check if the given input in @Cache is valid.
  bool isInputValid(ValueCache &Cache);
  Inst *Ante;
};

}

#endif
