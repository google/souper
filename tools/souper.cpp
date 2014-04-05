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

#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/PassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/DataStream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "klee/util/ExprSMTLIBLetPrinter.h"
#include "klee/Solver.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/CandidateMap.h"
#include "souper/SMTLIB2/Solver.h"

#include <iostream>

using namespace llvm;
using namespace souper;

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input bitcode file>"),
    cl::init("-"), cl::value_desc("filename"));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Override output filename"),
    cl::init(""), cl::value_desc("filename"));

static cl::opt<std::string> BoolectorPath(
    "boolector-path", cl::desc("Path to Boolector executable"), cl::init(""),
    cl::value_desc("path"));

static cl::opt<std::string> CVC4Path("cvc4-path",
                                     cl::desc("Path to CVC4 executable"),
                                     cl::init(""), cl::value_desc("path"));

static cl::opt<std::string> STPPath("stp-path",
                                    cl::desc("Path to STP executable"),
                                    cl::init(""), cl::value_desc("path"));

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);

  // Enable debug stream buffering.
  EnableDebugBuffering = true;

  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
  LLVMContext &Context = getGlobalContext();

  cl::ParseCommandLineOptions(argc, argv, "LLVM superoptimizer\n");

  std::string DisplayFilename;
  if (InputFilename == "-")
    DisplayFilename = "<stdin>";
  else
    DisplayFilename = InputFilename;

  std::string ErrorMessage;
  std::unique_ptr<Module> M;

  // Use the bitcode streaming interface
  DataStreamer *streamer = getDataFileStreamer(InputFilename, &ErrorMessage);
  if (streamer) {
    M.reset(getStreamedBitcodeModule(DisplayFilename, streamer, Context,
                                     &ErrorMessage));
    if (M.get() != 0) {
      if (error_code EC = M->materializeAllPermanently()) {
        ErrorMessage = EC.message();
        M.reset();
      }
    }
  }

  if (M.get() == 0) {
    if (ErrorMessage.size())
      llvm::errs() << ErrorMessage;
    else
      llvm::errs() << "Bitcode did not read correctly";
    return 1;
  }

  std::unique_ptr<SMTLIBSolver> Solver;
  if (!BoolectorPath.empty()) {
    Solver = createBoolectorSolver(makeExternalSolverProgram(BoolectorPath));
  } else if (!CVC4Path.empty()) {
    Solver = createCVC4Solver(makeExternalSolverProgram(CVC4Path));
  } else if (!STPPath.empty()) {
    Solver = createSTPSolver(makeExternalSolverProgram(STPPath));
  }

  std::vector<ExprCandidate> Cands = ExtractExprCandidates(M.get());
  ExprCandidateMap CandMap;
  AddToCandidateMap(CandMap, Cands);

  for (const auto &Cand : CandMap) {
    llvm::outs() << Cand.first();

    llvm::outs() << "Priority: " << Cand.second.Priority << "\n\n";
    llvm::outs() << "Functions:\n";
    for (const auto &F : Cand.second.Functions) {
      llvm::outs() << F.first() << '\n';
    }
    llvm::outs() << '\n';

    if (Solver) {
      bool Sat;
      if (error_code EC = Solver->isSatisfiable(Cand.first(), Sat)) {
        llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
        return 1;
      }
      if (Sat) {
        llvm::outs() << "sat\n\n";
      } else {
        llvm::outs() << "unsat\n\n";
      }
    }
  }

  return 0;
}
