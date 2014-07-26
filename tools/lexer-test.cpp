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
  auto MB = MemoryBuffer::getFileOrSTDIN(argc >= 2 ? argv[1] : "-");
  if (MB) {
    TestLexer(MB.get()->getBuffer());
  } else {
    llvm::errs() << MB.getError().message() << '\n';
  }
  return 0;
}
