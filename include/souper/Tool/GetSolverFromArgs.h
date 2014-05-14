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

#ifndef SOUPER_TOOL_GETSOLVERFROMARGS_H
#define SOUPER_TOOL_GETSOLVERFROMARGS_H

#include <memory>
#include <string>
#include "llvm/Support/CommandLine.h"
#include "souper/SMTLIB2/Solver.h"

namespace souper {

static llvm::cl::opt<std::string> BoolectorPath(
    "boolector-path", llvm::cl::desc("Path to Boolector executable"),
    llvm::cl::init(""), llvm::cl::value_desc("path"));

static llvm::cl::opt<std::string> CVC4Path(
    "cvc4-path", llvm::cl::desc("Path to CVC4 executable"), llvm::cl::init(""),
    llvm::cl::value_desc("path"));

static llvm::cl::opt<std::string> STPPath(
    "stp-path", llvm::cl::desc("Path to STP executable"), llvm::cl::init(""),
    llvm::cl::value_desc("path"));

static llvm::cl::opt<std::string> Z3Path(
    "z3-path", llvm::cl::desc("Path to Z3 executable"), llvm::cl::init(""),
    llvm::cl::value_desc("path"));

static llvm::cl::opt<bool> KeepSolverInputs(
    "keep-solver-inputs", llvm::cl::desc("Do not clean up solver inputs"),
    llvm::cl::init(false));

static std::unique_ptr<SMTLIBSolver> GetSolverFromArgs() {
  if (!BoolectorPath.empty()) {
    return createBoolectorSolver(makeExternalSolverProgram(BoolectorPath),
                                 KeepSolverInputs);
  } else if (!CVC4Path.empty()) {
    return createCVC4Solver(makeExternalSolverProgram(CVC4Path),
                            KeepSolverInputs);
  } else if (!STPPath.empty()) {
    return createSTPSolver(makeExternalSolverProgram(STPPath),
                           KeepSolverInputs);
  } else if (!Z3Path.empty()) {
    return createZ3Solver(makeExternalSolverProgram(Z3Path),
                          KeepSolverInputs);
  } else {
    return nullptr;
  }
}

}

#endif  // SOUPER_TOOL_GETSOLVERFROMARGS_H
