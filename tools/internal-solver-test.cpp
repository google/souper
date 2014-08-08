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

#include "llvm/Support/raw_ostream.h"
#include "souper/SMTLIB2/Solver.h"
#include <unistd.h>

using namespace souper;

int solver_main(int argc, char **argv) {
  sleep(10);
  puts("sat");
  return 0;
}

int main() {
  std::unique_ptr<SMTLIBSolver> Solver =
      createBoolectorSolver(makeInternalSolverProgram(solver_main), false);
  bool Sat;
  if (std::error_code EC =
          Solver->isSatisfiable("foo", Sat, 0, 0, /*Timeout=*/1)) {
    llvm::errs() << EC.message() << '\n';
    return 1;
  }
  llvm::outs() << (Sat ? "sat\n" : "unsat\n");
  return 0;
}
