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

#ifndef SOUPER_EXTRACTOR_CANDIDATEMAP_H
#define SOUPER_EXTRACTOR_CANDIDATEMAP_H

#include <string>
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"

namespace souper {

struct ExprCandidate;

struct ExprCandidateInfo {
  llvm::StringSet<> Functions;

  /// Cumulative priority of each candidate solvable with this query.
  unsigned Priority;
};

/// Map from queries to candidate information.
typedef llvm::StringMap<ExprCandidateInfo> ExprCandidateMap;

void AddToCandidateMap(ExprCandidateMap &M,
                       const std::vector<ExprCandidate> &Cands);

}

#endif  // SOUPER_EXTRACTOR_CANDIDATEMAP_H
