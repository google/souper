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

#include <map>
#include <memory>
#include <vector>
#include "klee/Expr.h"
#include "klee/util/Ref.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Inst/Inst.h"

namespace llvm {

class BasicBlock;
class LoopInfo;
class Function;
class Instruction;
class Value;

}

namespace souper {

/// A mapping from an Inst to a replacement. This may either represent a
/// path condition or a candidate replacement.
struct InstMapping {
  InstMapping() : Source(0), Replacement(0) {}
  InstMapping(Inst *Source, Inst *Replacement)
      : Source(Source), Replacement(Replacement) {}

  Inst *Source, *Replacement;
};

struct BlockCandidateSet;

struct CandidateReplacement {
  CandidateReplacement(llvm::Instruction *Origin, InstMapping Mapping,
                       unsigned Priority)
      : Origin(Origin), Mapping(Mapping), Priority(Priority) {}

  /// The instruction from which the candidate was derived.
  llvm::Instruction *Origin;

  /// The replacement mapping.
  InstMapping Mapping;

  /// The path conditions relevant to this replacement.
  std::vector<InstMapping> PCs;

  /// The priority of this replacement, i.e. the number of instructions saved by
  /// performing the replacement.
  unsigned Priority;

  void print(llvm::raw_ostream &Out) const;
};

void PrintReplacement(llvm::raw_ostream &Out,
                      const std::vector<InstMapping> &PCs, InstMapping Mapping);

struct BlockCandidateSet {
  llvm::BasicBlock *Origin;

  std::vector<InstMapping> PCs;
  std::vector<CandidateReplacement> Replacements;
};

struct FunctionCandidateSet {
  std::vector<std::unique_ptr<BlockCandidateSet>> Blocks;
};

struct ExprBuilderOptions {
  /// Whether arrays in the query are named after the corresponding LLVM values.
  /// Note that this option is currently not guaranteed to work correctly as it
  /// does not perform escaping. It should only be used for debugging or with
  /// controlled IR input (i.e. the unit tests).
  bool NamedArrays;

  ExprBuilderOptions() : NamedArrays(false) {}
};

struct BlockInfo {
  Block *B;

  // Each phi derived from this block must visit the predecessors in the same
  // order, as a consumer may wish to use the same predicates to control each phi.
  // This vector stores the blocks in the order observed in the first phi node
  // we visit in the block. This allows us to write deterministic tests by
  // controlling the order in which predecessors appear in each phi.
  std::vector<llvm::BasicBlock *> Preds;
};

struct ExprBuilderContext {
  std::map<const llvm::Value *, Inst *> InstMap;
  std::map<llvm::BasicBlock *, BlockInfo> BlockMap;
};

FunctionCandidateSet ExtractCandidatesFromPass(
    llvm::Function *F, const llvm::LoopInfo *LI, InstContext &IC,
    ExprBuilderContext &EBC,
    const ExprBuilderOptions &Opts = ExprBuilderOptions());

FunctionCandidateSet ExtractCandidates(
    llvm::Function *F, InstContext &IC, ExprBuilderContext &EBC,
    const ExprBuilderOptions &Opts = ExprBuilderOptions());

}

#endif  // SOUPER_EXTRACTOR_CANDIDATES_H
