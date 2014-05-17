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

#ifndef SOUPER_SMTLIB2_SOLVER_H
#define SOUPER_SMTLIB2_SOLVER_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/system_error.h"
#include <functional>
#include <memory>
#include <vector>

namespace souper {

typedef std::function<
    int(const std::vector<std::string> &Args, llvm::StringRef RedirectIn,
        llvm::StringRef RedirectOut, unsigned Timeout)> SolverProgram;

class SMTLIBSolver {
public:
  virtual ~SMTLIBSolver();
  virtual std::string getName() const = 0;
  virtual llvm::error_code isSatisfiable(llvm::StringRef Query, bool &Result,
                                         unsigned Timeout = 0) = 0;
};

SolverProgram makeExternalSolverProgram(llvm::StringRef Path);
SolverProgram makeInternalSolverProgram(int MainPtr(int argc, char **argv));

std::unique_ptr<SMTLIBSolver> createBoolectorSolver(SolverProgram Prog,
                                                    bool Keep);
std::unique_ptr<SMTLIBSolver> createCVC4Solver(SolverProgram Prog, bool Keep);
std::unique_ptr<SMTLIBSolver> createSTPSolver(SolverProgram Prog, bool Keep);
std::unique_ptr<SMTLIBSolver> createZ3Solver(SolverProgram Prog, bool Keep);
std::unique_ptr<SMTLIBSolver> createCachingSolver(std::unique_ptr<SMTLIBSolver> ActualSolver);

}

#endif // SOUPER_SMTLIB2_SOLVER_H
