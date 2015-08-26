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

#include "klee/Solver.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/DataStream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "souper/Extractor/Candidates.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include <iostream>

using namespace llvm;
using namespace souper;

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input bitcode file>"),
    cl::init("-"), cl::value_desc("filename"));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Override output filename"),
    cl::init(""), cl::value_desc("filename"));

static cl::opt<bool> StaticProfile("souper-static-profile", cl::init(false),
    cl::desc("Static profiling of Souper optimizations (default=false)"));

static cl::opt<bool>
Check("check", cl::desc("Check input for expected results"),
    cl::init(false));

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
  if (auto streamer = getDataFileStreamer(InputFilename, &ErrorMessage)) {
    auto ModOrError = getStreamedBitcodeModule(DisplayFilename,
                                               std::move(streamer), Context);
    if (auto EC = ModOrError.getError())
      ErrorMessage = EC.message();
    else
      M = std::move(ModOrError.get());
  }

  if (M.get() == 0) {
    if (ErrorMessage.size())
      llvm::errs() << ErrorMessage;
    else
      llvm::errs() << "Bitcode did not read correctly";
    return 1;
  }

  KVStore *KV = 0;
  std::unique_ptr<Solver> S = GetSolverFromArgs(KV);

  InstContext IC;
  ExprBuilderContext EBC;
  CandidateMap CandMap;

  AddModuleToCandidateMap(IC, EBC, CandMap, M.get());

  if (Check) {
    return CheckCandidateMap(*M.get(), CandMap, S.get(), IC) ? 0 : 1;
  } else {
    if (StaticProfile && !KV)
      KV = new KVStore;
    return SolveCandidateMap(llvm::outs(), CandMap, S.get(), IC,
                             StaticProfile ? KV : 0) ? 0 : 1;
  }
}
