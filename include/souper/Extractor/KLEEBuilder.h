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

#ifndef SOUPER_EXTRACTOR_KLEEBUILDER_H
#define SOUPER_EXTRACTOR_KLEEBUILDER_H

#include "klee/Expr.h"
#include "klee/util/Ref.h"
#include "llvm/ADT/Optional.h"
#include "souper/Extractor/Candidates.h"
#include <memory>
#include <vector>

namespace souper {

struct CandidateExpr {
  std::vector<std::unique_ptr<klee::Array>> Arrays;
  std::vector<Inst *> ArrayVars;
  klee::ref<klee::Expr> E;
};

llvm::Optional<CandidateExpr> GetCandidateExprForReplacement(
    const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
    InstMapping Mapping, bool Negate);

std::string BuildQuery(const BlockPCs &BPCs,
                       const std::vector<InstMapping> &PCs, InstMapping Mapping,
                       std::vector<Inst *> *ModelVars, bool Negate=false);

}

#endif  // SOUPER_EXTRACTOR_KLEEBUILDER_H
