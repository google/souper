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

#ifndef SOUPER_CLANGTOOL_ACTIONS_H
#define SOUPER_CLANGTOOL_ACTIONS_H

#include "souper/Tool/CandidateMapUtils.h"
#include <functional>

namespace llvm {

class LLVMContext;
class Module;

}

namespace clang {
namespace tooling {

class FrontendActionFactory;

}
}

namespace souper {

struct ExprBuilderContext;
class InstContext;

clang::tooling::FrontendActionFactory *CreateExtractorActionFactory(
    llvm::LLVMContext &VMC, InstContext &IC, ExprBuilderContext &EBC,
    std::vector<std::unique_ptr<llvm::Module>> &Mods, CandidateMap &CandMap);

}

#endif  // SOUPER_CLANGTOOL_ACTIONS_H
