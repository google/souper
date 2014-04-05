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
#include "llvm/ADT/OwningPtr.h"
#include "llvm/IR/Module.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/CandidateMap.h"
#include "souper/SMTLIB2/Solver.h"
#include <functional>
#include <memory>

using namespace llvm;
using namespace clang;
using namespace souper;

namespace {

class ExtractorAction : public CodeGenAction {
  ExprCandidateMap &CandMap;
  PerTranslationUnitCallback CB;

 public:
  ExtractorAction(ExprCandidateMap &CandMap, PerTranslationUnitCallback CB)
      : CodeGenAction(Backend_EmitNothing), CandMap(CandMap), CB(CB) {}

  void EndSourceFileAction() {
    CodeGenAction::EndSourceFileAction();
    std::unique_ptr<Module> Mod(takeModule());

    if (Mod) {
      auto Cands = ExtractExprCandidates(Mod.get());
      AddToCandidateMap(CandMap, Cands);
      if (CB)
        CB(CandMap);
    }
  }
};

class ExtractorActionFactory : public tooling::FrontendActionFactory {
  ExprCandidateMap &CandMap;
  PerTranslationUnitCallback CB;

 public:
  ExtractorActionFactory(ExprCandidateMap& CandMap,
                         PerTranslationUnitCallback CB)
      : CandMap(CandMap), CB(CB) {}

  FrontendAction* create() {
    return new ExtractorAction(CandMap, CB);
  }
};

}

tooling::FrontendActionFactory *souper::CreateExtractorActionFactory(
    ExprCandidateMap &CandMap, PerTranslationUnitCallback CB) {
  return new ExtractorActionFactory(CandMap, CB);
}
