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

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"
#include "souper/ClangTool/Actions.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "souper/Tool/GetSolverFromArgs.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace souper;

static cl::opt<std::string> BuildPath(cl::Positional, cl::desc("<build-path>"));

static cl::list<std::string> SourcePaths(cl::Positional,
                                         cl::desc("<source0> [... <sourceN>]"),
                                         cl::OneOrMore);

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  cl::ParseCommandLineOptions(argc, argv);

  std::unique_ptr<CompilationDatabase> Compilations(
      FixedCompilationDatabase::loadFromCommandLine(argc, argv));
  if (!Compilations) {  // Couldn't find a compilation DB from the command line
    std::string ErrorMessage;
    Compilations.reset(
      !BuildPath.empty() ?
        CompilationDatabase::autoDetectFromDirectory(BuildPath, ErrorMessage) :
        CompilationDatabase::autoDetectFromSource(SourcePaths[0], ErrorMessage)
      );

    // Still no compilation DB? - bail.
    if (!Compilations)
      llvm::report_fatal_error(ErrorMessage);
  }

  ClangTool Tool(*Compilations, SourcePaths);
  InstContext IC;
  ExprBuilderContext EBC;
  CandidateMap CandMap;
  std::vector<std::unique_ptr<llvm::Module>> OwnedMods;
  std::unique_ptr<FrontendActionFactory> Factory(CreateExtractorActionFactory(
      getGlobalContext(), IC, EBC, OwnedMods, CandMap));
  Tool.run(Factory.get());

  std::unique_ptr<SMTLIBSolver> Solver = GetSolverFromArgs();
  return SolveCandidateMap(llvm::outs(), CandMap, Solver.get()) ? 0 : 1;
}
