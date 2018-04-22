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

#ifndef SOUPER_TOOL_CANDIDATEMAPUTILS_H
#define SOUPER_TOOL_CANDIDATEMAPUTILS_H

#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/ExprBuilder.h"
#include "souper/Extractor/Solver.h"
#include "souper/KVStore/KVStore.h"

namespace llvm {

class Module;

}

namespace souper {

class Solver;

typedef std::vector<CandidateReplacement> CandidateMap;

void AddToCandidateMap(CandidateMap &M, const CandidateReplacement &CR);

void AddModuleToCandidateMap(InstContext &IC, ExprBuilderContext &EBC,
                             CandidateMap &CandMap, llvm::Module *M);

bool SolveCandidateMap(llvm::raw_ostream &OS, CandidateMap &M,
                       Solver *Solver, InstContext &IC,
                       KVStore *KVForStaticProfile);

bool CheckCandidateMap(llvm::Module &Mod, CandidateMap &M, Solver *S,
                       InstContext &IC);

}

#endif  // SOUPER_TOOL_CANDIDATEMAPUTILS_H
