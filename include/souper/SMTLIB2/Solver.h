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

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringRef.h"
#include <functional>
#include <memory>
#include <system_error>
#include <vector>

namespace souper {

typedef std::function<
    int(const std::vector<std::string> &Args, llvm::StringRef RedirectIn,
        llvm::StringRef RedirectOut, llvm::StringRef RedirectErr,
        unsigned Timeout)> SolverProgram;

class SMTLIBSolver {
public:
  virtual ~SMTLIBSolver();
  virtual std::string getName() const = 0;
  virtual bool supportsModels() const = 0;
  virtual std::error_code isSatisfiable(llvm::StringRef Query, bool &Result,
                                        unsigned NumModels,
                                        std::vector<llvm::APInt> *Models,
                                        unsigned Timeout = 0) = 0;
};

SolverProgram makeExternalSolverProgram(llvm::StringRef Path);
SolverProgram makeInternalSolverProgram(int MainPtr(int argc, char **argv));

std::unique_ptr<SMTLIBSolver> createBoolectorSolver(SolverProgram Prog,
                                                    bool Keep);
std::unique_ptr<SMTLIBSolver> createCVC4Solver(SolverProgram Prog, bool Keep);
std::unique_ptr<SMTLIBSolver> createSTPSolver(SolverProgram Prog, bool Keep);
std::unique_ptr<SMTLIBSolver> createZ3Solver(SolverProgram Prog, bool Keep);

}

#endif // SOUPER_SMTLIB2_SOLVER_H
