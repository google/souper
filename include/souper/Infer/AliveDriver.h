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
  AliveDriver(Inst *LHS_, Inst *PreCondition_);

  std::optional<int64_t> synthesizeConstant(souper::Inst *RHS);

  bool verify(Inst *RHS);
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
  IR::Function LHSF;

  int InstNumbers;
  std::unordered_map<const Inst *, std::string> NamesCache;

  smt::smt_initializer smt_init;
};

}

#endif
