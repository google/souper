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

#ifndef SOUPER_UTIL_ALIVE_DRIVER_H
#define SOUPER_UTIL_ALIVE_DRIVER_H

#include "souper/Infer/Interpreter.h"
#include "souper/Inst/Inst.h"
#include "alive2/ir/function.h"
#include "alive2/smt/smt.h"

#include <unordered_map>
#include <optional>

namespace souper {

// TODO: Rename to AliveBuilder if we implement the ExprBuilder API in future
class AliveDriver {
  typedef std::unordered_map<const Inst *, IR::Value *> Cache;
public:
  AliveDriver(Inst *LHS_, Inst *PreCondition_, InstContext &IC_);

  std::map<Inst *, llvm::APInt> synthesizeConstants(souper::Inst *RHS);
  std::map<Inst *, llvm::APInt> synthesizeConstantsWithCegis(souper::Inst *RHS, InstContext &IC);

  bool verify(Inst *RHS, Inst *RHSAssumptions = nullptr);
  ~AliveDriver() {
    for (auto &&p : TypeCache) {
      delete(p.second);
    }
  }
private:
  Inst *LHS, *PreCondition;

  Cache LExprCache, RExprCache;

  std::unordered_map<int, IR::Type*> TypeCache;

  IR::Type &getType(int n);

  bool translateRoot(const Inst *I, const Inst *PC, IR::Function &F, Cache &ExprCache);
  bool translateAndCache(const Inst *I, IR::Function &F, Cache &ExprCache);
  bool translateDataflowFacts(const Inst *I, IR::Function &F, Cache &ExprCache);
  IR::Function LHSF;

  int InstNumbers;
  std::unordered_map<const Inst *, std::string> NamesCache;

  InstContext &IC;
  smt::smt_initializer smt_init;
};

bool isTransformationValid(Inst* LHS, Inst* RHS, const std::vector<InstMapping> &PCs,
                           InstContext &IC);

bool isCandidateInfeasible(Inst *RHS, ValueCache &C, llvm::APInt LHSValue,
                          InstContext &IC);

}

#endif
