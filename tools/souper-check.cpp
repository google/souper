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

#include "llvm/Support/MemoryBuffer.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/GetSolverFromArgs.h"

using namespace llvm;
using namespace souper;

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input souper optimization>"),
              cl::init("-"));

static cl::opt<bool> PrintCounterExample("print-counterexample",
    cl::desc("Print counterexample (default=true)"),
    cl::init(true));

static cl::opt<bool> PrintRepl("print-replacement",
    cl::desc("Print the replacement, if valid (default=false)"),
    cl::init(false));

static cl::opt<bool> PrintReplSplit("print-replacement-split", cl::Hidden,
    cl::desc("Print the replacement as an infer/result pair (default=false)"),
    cl::init(false));

static cl::opt<bool> InferRHS("infer-rhs",
    cl::desc("Try to infer a RHS for a Souper LHS (default=false)"),
    cl::init(false));

int SolveInst(const MemoryBufferRef &MB, Solver *S) {
  InstContext IC;
  std::string ErrStr;

  ParsedReplacement Rep;
  ReplacementContext Context;
  if (InferRHS) {
    Rep = ParseReplacementLHS(IC, MB.getBufferIdentifier(), MB.getBuffer(),
                              Context, ErrStr);
  } else {
    Rep = ParseReplacement(IC, MB.getBufferIdentifier(), MB.getBuffer(), ErrStr);
  }
  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  if (InferRHS) {
    if (std::error_code EC = S->infer(Rep.PCs, Rep.Mapping.LHS, Rep.Mapping.RHS,
                                      IC)) {
      llvm::errs() << EC.message() << '\n';
      return 1;
    }
    if (Rep.Mapping.RHS) {
      llvm::outs() << "; RHS inferred successfully\n";
      if (PrintRepl) {
        PrintReplacement(llvm::outs(), Rep.PCs, Rep.Mapping);
      } else if (PrintReplSplit) {
        ReplacementContext Context;
        PrintReplacementLHS(llvm::outs(), Rep.PCs, Rep.Mapping.LHS, Context);
        PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS, Context);
      } else {
        ReplacementContext Context;
        PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS, Context);
      }
    } else {
      llvm::outs() << "; Failed to infer RHS\n";
      if (PrintRepl || PrintReplSplit) {
        ReplacementContext Context;
        PrintReplacementLHS(llvm::outs(), Rep.PCs, Rep.Mapping.LHS, Context);
      }
    }
  } else {
    bool Valid;
    std::vector<std::pair<Inst *, APInt>> Models;
    if (std::error_code EC = S->isValid(Rep.PCs, Rep.Mapping, Valid, &Models)) {
      llvm::errs() << EC.message() << '\n';
      return 1;
    }

    if (Valid) {
      llvm::outs() << "; LGTM\n";
      if (PrintRepl)
        PrintReplacement(llvm::outs(), Rep.PCs, Rep.Mapping);
      if (PrintReplSplit) {
        ReplacementContext Context;
        PrintReplacementLHS(llvm::outs(), Rep.PCs, Rep.Mapping.LHS, Context);
        PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS, Context);
      }
    } else {
      llvm::outs() << "Invalid";
      if (PrintCounterExample && !Models.empty()) {
        llvm::outs() << ", e.g.\n\n";
        std::sort(Models.begin(), Models.end(),
                  [](const std::pair<Inst *, APInt> &A,
                     const std::pair<Inst *, APInt> &B) {
                    return A.first->Name < B.first->Name;
                  });
        for (const auto &M : Models) {
          llvm::outs() << '%' << M.first->Name << " = " << M.second << '\n';
        }
      } else {
        llvm::outs() << "\n";
      }
    }
  }
  return 0;
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);
  KVStore *KV = 0;
  std::unique_ptr<Solver> S = GetSolverFromArgs(KV);
  if (!S) {
    llvm::errs() << "Specify a solver\n";
    return 1;
  }

  auto MB = MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!MB) {
    llvm::errs() << MB.getError().message() << '\n';
    return 1;
  }
  return SolveInst((*MB)->getMemBufferRef(), S.get());
}
