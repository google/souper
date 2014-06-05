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

#include "souper/Extractor/Solver.h"
#include <map>
#include <string>
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/KLEEBuilder.h"
#include "llvm/Support/system_error.h"
#include "souper/SMTLIB2/Solver.h"
#include "klee/Expr.h"
#include "klee/Solver.h"
#include "klee/util/Ref.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/util/ExprSMTLIBLetPrinter.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/KLEEBuilder.h"

using namespace souper;

namespace {

class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

  llvm::error_code isValid(const CandidateMapEntry &E, bool &IsValid) {
    bool IsSat;
    llvm::error_code EC = SMTSolver->isSatisfiable(E.getQuery(), IsSat, Timeout);
    IsValid = !IsSat;
    return EC;
  }

  std::string getName() {
    return SMTSolver->getName();
  }
};

class CachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;

public:
  CachingSolver(std::unique_ptr<Solver> UnderlyingSolver)
      : UnderlyingSolver(std::move(UnderlyingSolver)) {}

  llvm::error_code isValid(const CandidateMapEntry &E, bool &IsValid) {
    // FIXME: Make this do caching.
    return UnderlyingSolver->isValid(E, IsValid);
  }

  std::string getName() {
    return "caching + " + UnderlyingSolver->getName();
  }

};

}

namespace souper {

Solver::~Solver() {}

std::unique_ptr<Solver> createBaseSolver(
    std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout) {
  return std::unique_ptr<Solver>(new BaseSolver(std::move(SMTSolver), Timeout));
}

std::unique_ptr<Solver> createCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver) {
  return std::unique_ptr<Solver>(
      new CachingSolver(std::move(UnderlyingSolver)));
}

}
