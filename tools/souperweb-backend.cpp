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
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Solver.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "souper/Tool/GetSolverFromArgs.h"

using namespace llvm;
using namespace souper;

extern "C" int boolector_main(int argc, char **argv);

LLVMContext Context;

void SolveIR(std::unique_ptr<MemoryBuffer> MB, Solver *S) {
  SMDiagnostic Err;
  if (std::unique_ptr<Module> M =
          parseAssembly(MB->getMemBufferRef(), Err, Context)) {
    InstContext IC;
    ExprBuilderContext EBC;
    CandidateMap CandMap;

    AddModuleToCandidateMap(IC, EBC, CandMap, M.get());

    SolveCandidateMap(llvm::outs(), CandMap, S, IC, 0);
  } else {
    Err.print(0, llvm::errs(), false);
  }
}

void SolveInst(std::unique_ptr<MemoryBuffer> MB, Solver *S) {
  InstContext IC;
  std::string ErrStr;

  ParsedReplacement Rep =
      ParseReplacement(IC, "<input>", MB->getBuffer(), ErrStr);
  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return;
  }

  bool Valid;
  std::vector<std::pair<Inst *, APInt>> Models;
  if (std::error_code EC = S->isValid(Rep.BPCs, Rep.PCs,
                                      Rep.Mapping, Valid, &Models)) {
    llvm::errs() << EC.message() << '\n';
    return;
  }

  if (Valid) {
    llvm::outs() << "LGTM\n";
  } else {
    llvm::outs() << "Invalid";
    if (!Models.empty()) {
      llvm::outs() << ", e.g.\n\n";
      std::sort(Models.begin(), Models.end(),
                [](const std::pair<Inst *, APInt> &A,
                   const std::pair<Inst *, APInt> &B) {
        return A.first->Name < B.first->Name;
      });
      for (const auto &M : Models) {
        llvm::outs() << '%' << M.first->Name << " = " << M.second << '\n';
      }
    }
  }
}

static llvm::cl::opt<std::string> Action("action", llvm::cl::init(""));

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);
  KVStore *KV;
  std::unique_ptr<Solver> S = GetSolverFromArgs(KV);

  auto MB = MemoryBuffer::getSTDIN();
  if (MB) {
    if (Action == "ir") {
      SolveIR(std::move(*MB), S.get());
    } else {
      SolveInst(std::move(*MB), S.get());
    }
  } else {
    llvm::errs() << MB.getError().message() << '\n';
  }
}
