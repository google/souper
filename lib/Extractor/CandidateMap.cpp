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

#include "souper/Extractor/CandidateMap.h"

#include <vector>
#include "klee/Expr.h"
#include "klee/Solver.h"
#include "klee/util/Ref.h"
#include "klee/util/ExprSMTLIBLetPrinter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Module.h"
#include "souper/Extractor/Candidates.h"

using namespace souper;
using namespace klee;
using namespace llvm;

void souper::AddToCandidateMap(ExprCandidateMap &M,
                               const std::vector<ExprCandidate> &Cands) {
  for (const auto &Cand : Cands) {
    std::string FunctionName;
    const Function *F = Cand.Origin->getParent()->getParent();
    if (F->hasLocalLinkage()) {
      FunctionName =
          (F->getParent()->getModuleIdentifier() + ":" + F->getName()).str();
    } else {
      FunctionName = F->getName();
    }

    for (const auto &Q : Cand.Queries) {
      std::ostringstream SMTSS;
      ConstraintManager Manager;
      Query KQuery(Manager, Expr::createIsZero(Q.Expr));
      ExprSMTLIBLetPrinter Printer;
      Printer.setOutput(SMTSS);
      Printer.setQuery(KQuery);
      Printer.generateOutput();

      ExprCandidateInfo &Info = M[SMTSS.str()];
      Info.Functions.insert(FunctionName);
      Info.Priority += Q.Priority;
    }
  }
}
