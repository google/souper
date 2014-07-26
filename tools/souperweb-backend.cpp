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

#include "llvm/AsmParser/Parser.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Solver.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/CandidateMapUtils.h"

using namespace llvm;
using namespace souper;

extern "C" int boolector_main(int argc, char **argv);

void SolveIR(std::unique_ptr<MemoryBuffer> MB, Solver *S) {
  Module M("", getGlobalContext());
  SMDiagnostic Err;
  if (!ParseAssembly(MB.release(), &M, Err, getGlobalContext())) {
    Err.print(0, llvm::errs(), false);
  } else {
    InstContext IC;
    ExprBuilderContext EBC;
    CandidateMap CandMap;

    AddModuleToCandidateMap(IC, EBC, CandMap, &M);

    SolveCandidateMap(llvm::outs(), CandMap, S);
  }
}

void SolveInst(std::unique_ptr<MemoryBuffer> MB, Solver *S) {
  InstContext IC;
  std::string ErrStr;

  ParsedReplacement Rep =
      ParseReplacement(IC, "<input>", MB->getBuffer(), ErrStr);
  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
  } else {
    bool Valid;
    if (std::error_code EC = S->isValid(Rep.PCs, Rep.Mapping, Valid)) {
      llvm::errs() << EC.message() << '\n';
    } else {
      if (Valid) {
        llvm::outs() << "LGTM\n";
      } else {
        llvm::outs() << "Invalid\n";
      }
    }
  }
}

int main(int argc, char **argv) {
  std::unique_ptr<Solver> S = createBaseSolver(
      createBoolectorSolver(makeInternalSolverProgram(boolector_main), false),
      10);

  auto MB = MemoryBuffer::getSTDIN();
  if (MB) {
    if (StringRef(argv[1]) == "ir") {
      SolveIR(std::move(*MB), S.get());
    } else {
      SolveInst(std::move(*MB), S.get());
    }
  } else {
    llvm::errs() << MB.getError().message() << '\n';
  }
}
