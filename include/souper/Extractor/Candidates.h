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

#include "llvm/ADT/StringSet.h"
#include "llvm/Analysis/DemandedBits.h"
#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Inst/Inst.h"
#include <map>
#include <memory>
#include <vector>

namespace llvm {

class BasicBlock;
class LoopInfo;
class Function;
class Instruction;
class Value;

}

namespace souper {

struct CandidateReplacement {
  CandidateReplacement(llvm::Instruction *Origin, InstMapping Mapping)
  : Origin(Origin), Mapping(Mapping) {}

  /// The instruction from which the candidate was derived.
  llvm::Instruction *Origin;

  /// The replacement mapping.
  InstMapping Mapping;

  /// The path conditions relevant to this replacement.
  std::vector<InstMapping> PCs;

  /// The block path conditions relevant to this replacement.
  /// A BlockPC has the same semantics as a PC, except that the PC only applies
  /// if the given predecessor of the given block is chosen.
  BlockPCs BPCs;

  void printFunction(llvm::raw_ostream &Out) const;
  void printLHS(llvm::raw_ostream &Out, ReplacementContext &Context,
                bool printNames = false) const;
  void print(llvm::raw_ostream &Out,
             bool printNames = false) const;
};

struct BlockCandidateSet {
  std::vector<InstMapping> PCs;
  BlockPCs BPCs;
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
  // order, as a consumer may wish to use the same predicates to control each
  // phi. This vector stores the blocks in the order observed in the first phi
  // node we visit in the block. This allows us to write deterministic tests by
  // controlling the order in which predecessors appear in each phi.
  std::vector<llvm::BasicBlock *> Preds;
};

struct ExprBuilderContext {
  std::map<const llvm::Value *, Inst *> InstMap;
  std::map<llvm::BasicBlock *, BlockInfo> BlockMap;
};

FunctionCandidateSet ExtractCandidatesFromPass(
    llvm::Function *F, const llvm::LoopInfo *LI, llvm::DemandedBits *DB,
    llvm::LazyValueInfo *LVI, llvm::ScalarEvolution *SE,
    llvm::TargetLibraryInfo *TLI, InstContext &IC, ExprBuilderContext &EBC,
    const ExprBuilderOptions &Opts = ExprBuilderOptions());

FunctionCandidateSet ExtractCandidates(
    llvm::Function *F, InstContext &IC, ExprBuilderContext &EBC,
    const ExprBuilderOptions &Opts = ExprBuilderOptions());

}

#endif  // SOUPER_EXTRACTOR_CANDIDATES_H
