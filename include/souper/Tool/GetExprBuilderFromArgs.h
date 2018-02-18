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

#ifndef SOUPER_TOOL_GETEXPRBUILDERFROMARGS_H
#define SOUPER_TOOL_GETEXPRBUILDERFROMARGS_H

#include "llvm/Support/CommandLine.h"
#include "souper/Extractor/ExprBuilder.h"
#include <memory>

namespace souper {

static llvm::cl::opt<bool> UseKLEEExprBuilder(
    "use-klee-builder", llvm::cl::desc("Use KLEE Expr builder to emit SMTLIBv2"),
    llvm::cl::init(true));

static std::unique_ptr<ExprBuilder> GetExprBuilderFromArgs() {
  if (UseKLEEExprBuilder) {
    return createKLEEExprBuilder();
  } else {
    return nullptr;
  }
}

}

#endif  // SOUPER_TOOL_GETEXPRBUILDERFROMARGS_H
