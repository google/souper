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

#ifndef SOUPER_CONSTANT_SYNTHESIS_H
#define SOUPER_CONSTANT_SYNTHESIS_H

#include "llvm/ADT/APInt.h"
#include "souper/Extractor/Solver.h"
#include "souper/Inst/Inst.h"

#include <optional>
#include <utility>
#include <system_error>

namespace souper {

class PruningManager;

class ConstantSynthesis {
public:
  ConstantSynthesis(PruningManager *P = nullptr) : Pruner(P) {}

  // Synthesize a set of constants from the specification in LHS
  virtual std::error_code synthesize(SMTLIBSolver *SMTSolver,
                             const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             InstMapping Mapping, std::set <Inst *> &ConstSet,
                             std::map <Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC, unsigned MaxTries, unsigned Timeout,
                             bool AvoidNops);

protected:
  PruningManager *Pruner = nullptr;
};

class ConstantSynthesisZ3 : public ConstantSynthesis {
public:
  ConstantSynthesisZ3(PruningManager *P = nullptr) : ConstantSynthesis(P) {}

  std::error_code synthesize(SMTLIBSolver *SMTSolver,
                             const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             InstMapping Mapping, std::set <Inst *> &ConstSet,
                             std::map <Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC, unsigned MaxTries, unsigned Timeout,
                             bool AvoidNops) override;

};

}

#endif  // SOUPER_CONSTANT_SYNTHESIS_H
