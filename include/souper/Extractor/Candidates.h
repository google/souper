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

#ifndef SOUPER_EXTRACTOR_CANDIDATES_H
#define SOUPER_EXTRACTOR_CANDIDATES_H

#include <memory>
#include <vector>
#include "klee/Expr.h"
#include "klee/util/Ref.h"
#include "llvm/ADT/StringSet.h"

namespace llvm {

class Instruction;
class Module;

}

namespace souper {

struct ExprCandidateQuery {
  ExprCandidateQuery(klee::ref<klee::Expr> Expr, bool Replacement,
                     unsigned Priority)
      : Expr(Expr), Replacement(Replacement), Priority(Priority) {}

  /// The expression which we want to prove unsatisfiable.
  klee::ref<klee::Expr> Expr;

  /// If proven unsatisfiable, the value to replace Origin with.
  bool Replacement;

  /// The priority of this replacement, i.e. the number of instructions saved by
  /// performing the replacement.
  unsigned Priority;
};

struct ExprCandidate {
  llvm::Instruction *Origin;

  std::vector<std::unique_ptr<klee::Array>> Arrays;
  std::vector<ExprCandidateQuery> Queries;
};

struct ExprBuilderOptions {
  /// Whether arrays in the query are named after the corresponding LLVM values.
  /// Note that this option is currently not guaranteed to work correctly as it
  /// does not perform escaping. It should only be used for debugging or with
  /// controlled IR input (i.e. the unit tests).
  bool NamedArrays;

  ExprBuilderOptions() : NamedArrays(false) {}
};

std::vector<ExprCandidate> ExtractExprCandidates(
    llvm::Module *M, const ExprBuilderOptions &Opts = ExprBuilderOptions());
}

#endif  // SOUPER_EXTRACTOR_CANDIDATES_H
