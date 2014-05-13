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

#include <map>
#include <string>
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/KLEEBuilder.h"

namespace llvm {

class Instruction;

}

namespace souper {

struct CandidateReplacement;

struct CandidateMapEntry {
  /// The KLEE expr for this candidate.
  CandidateExpr CandExpr;

  std::vector<InstMapping> PCs;
  InstMapping Mapping;

  std::vector<llvm::Instruction *> Origins;

  /// Cumulative priority of each instruction for which this candidate applies.
  unsigned Priority;

  void print(llvm::raw_ostream &OS) const;

  /// Return the SMT-LIB2 query for this candidate.
  std::string getQuery() const;
};

/// Map from candidate string representations to candidate information.
typedef std::map<std::string, CandidateMapEntry> CandidateMap;

void AddToCandidateMap(CandidateMap &M, const CandidateReplacement &CR);
void PrintCandidateMap(llvm::raw_ostream &OS, CandidateMap &M);

}

#endif  // SOUPER_EXTRACTOR_CANDIDATEMAP_H
