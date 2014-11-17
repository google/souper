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
#include "llvm/Support/MemoryBuffer.h"
#include "souper/Parser/Parser.h"
#include <unistd.h>

using namespace souper;
using namespace llvm;

int main(int argc, char **argv) {
  int Arg = 1, LHSOnly = 0;
  if (Arg < argc && strcmp(argv[Arg], "-LHS") == 0) {
    LHSOnly = 1;
    ++Arg;
  }
  auto MB = MemoryBuffer::getFileOrSTDIN(argc >= (Arg+1) ? argv[Arg] : "-");
  if (MB) {
    InstContext IC;
    std::string ErrStr;
    std::vector<ParsedReplacement> Reps;
    std::vector<ReplacementContext> Contexts;
    if (LHSOnly)
      Reps = ParseReplacementLHSs(IC, MB.get()->getBufferIdentifier(),
                                  MB.get()->getBuffer(), Contexts, ErrStr);
    else
      Reps = ParseReplacements(IC, MB.get()->getBufferIdentifier(),
                               MB.get()->getBuffer(), ErrStr);
    if (!ErrStr.empty()) {
      llvm::errs() << ErrStr << '\n';
      return 1;
    }

    for (const auto &R : Reps) {
      if (LHSOnly) {
        ReplacementContext Context;
        R.printLHS(llvm::outs(), Context);
      } else {
        R.print(llvm::outs());
      }
    }
  } else {
    llvm::errs() << MB.getError().message() << '\n';
  }
  return 0;
}
