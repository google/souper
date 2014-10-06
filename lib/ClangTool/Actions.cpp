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

#include "souper/ClangTool/Actions.h"

#include "clang/CodeGen/BackendUtil.h"
#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/IR/Module.h"
#include "souper/Extractor/Candidates.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/CandidateMapUtils.h"
#include <functional>
#include <memory>

using namespace llvm;
using namespace clang;
using namespace souper;

namespace {

class ExtractorAction : public CodeGenAction {
  InstContext &IC;
  ExprBuilderContext &EBC;
  std::vector<std::unique_ptr<llvm::Module>> &Mods;
  CandidateMap &CandMap;

public:
  ExtractorAction(LLVMContext &VMC, InstContext &IC, ExprBuilderContext &EBC,
                  std::vector<std::unique_ptr<llvm::Module>> &Mods,
                  CandidateMap &CandMap)
      : CodeGenAction(Backend_EmitNothing, &VMC),
        IC(IC),
        EBC(EBC),
        Mods(Mods),
        CandMap(CandMap) {}

  void EndSourceFileAction() {
    CodeGenAction::EndSourceFileAction();
    std::unique_ptr<llvm::Module> Mod(takeModule());

    if (Mod) {
      AddModuleToCandidateMap(IC, EBC, CandMap, Mod.get());
      Mods.emplace_back(std::move(Mod));
    }
  }
};

class ExtractorActionFactory : public tooling::FrontendActionFactory {
  LLVMContext &VMC;
  InstContext &IC;
  ExprBuilderContext &EBC;
  std::vector<std::unique_ptr<llvm::Module>> &Mods;
  CandidateMap &CandMap;

 public:
  ExtractorActionFactory(LLVMContext &VMC, InstContext &IC,
                         ExprBuilderContext &EBC,
                         std::vector<std::unique_ptr<llvm::Module>> &Mods,
                         CandidateMap &CandMap)
      : VMC(VMC), IC(IC), EBC(EBC), Mods(Mods), CandMap(CandMap) {}

  FrontendAction *create() {
    return new ExtractorAction(VMC, IC, EBC, Mods, CandMap);
  }
};

}

tooling::FrontendActionFactory *souper::CreateExtractorActionFactory(
    LLVMContext &VMC, InstContext &IC, ExprBuilderContext &EBC,
    std::vector<std::unique_ptr<llvm::Module>> &Mods, CandidateMap &CandMap) {
  return new ExtractorActionFactory(VMC, IC, EBC, Mods, CandMap);
}
