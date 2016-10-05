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
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "souper/ClangTool/Actions.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include <system_error>

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace souper;

static cl::OptionCategory ClangSouperCategory("clang-souper options");

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
  CommonOptionsParser OptionsParser(argc, argv, ClangSouperCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  InstContext IC;
  ExprBuilderContext EBC;
  CandidateMap CandMap;
  std::vector<std::unique_ptr<llvm::Module>> OwnedMods;
  LLVMContext Context;
  std::unique_ptr<FrontendActionFactory> Factory(CreateExtractorActionFactory(
      Context, IC, EBC, OwnedMods, CandMap));
  Tool.run(Factory.get());

  KVStore *KV = 0;
  std::unique_ptr<Solver> S = GetSolverFromArgs(KV);
  return SolveCandidateMap(llvm::outs(), CandMap, S.get(), IC, 0) ? 0 : 1;
}
