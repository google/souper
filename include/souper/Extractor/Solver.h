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

#ifndef SOUPER_EXTRACTOR_SOLVER_H
#define SOUPER_EXTRACTOR_SOLVER_H

#include <vector>
#include "llvm/Support/system_error.h"
#include "souper/Extractor/Candidates.h"
#include "souper/SMTLIB2/Solver.h"

namespace souper {

class Solver {
public:
  virtual llvm::error_code isValid(const std::vector<InstMapping> &PCs,
                                   InstMapping Mapping, bool &IsValid) = 0;
};

std::unique_ptr<Solver> createBaseSolver(
    std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout);
std::unique_ptr<Solver> createCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver);

}

#endif  // SOUPER_EXTRACTOR_SOLVER_H
