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

#include "souper/Codegen/Codegen.h"
#include "souper/Parser/Parser.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace llvm;
using namespace souper;

unsigned DebugLevel;

static cl::opt<unsigned, /*ExternalStorage=*/true>
DebugFlagParser("souper-debug-level",
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."),
     cl::location(DebugLevel), cl::init(1));

static cl::opt<std::string>
    InputFilename(cl::Positional,
                  cl::desc("<input souper RHS (default=stdin)>"),
                  cl::init("-"));

static cl::opt<std::string> OutputFilename(
    "o", cl::desc("<output destination for textual LLVM IR (default=stdout)>"),
    cl::init("-"));

int Work(const MemoryBufferRef &MB) {
  InstContext IC;
  ReplacementContext RC;
  std::string ErrStr;

  const ParsedReplacement &RepRHS = ParseReplacementRHS(
      IC, MB.getBufferIdentifier(), MB.getBuffer(), RC, ErrStr);

  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  llvm::LLVMContext Context;
  llvm::Module Module("souper.ll", Context);
  if (genModule(IC, RepRHS.Mapping.RHS, Module))
    return 1;

  std::error_code EC;
  llvm::raw_fd_ostream OS(OutputFilename, EC);
  OS << Module;
  OS.flush();

  return 0;
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);

  auto MB = MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!MB) {
    llvm::errs() << MB.getError().message() << '\n';
    return 1;
  }

  return Work((*MB)->getMemBufferRef());
}
