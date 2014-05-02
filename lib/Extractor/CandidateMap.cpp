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
#include "klee/util/ExprPPrinter.h"
#include "klee/util/ExprSMTLIBLetPrinter.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/KLEEBuilder.h"

static llvm::cl::opt<bool> DumpKLEEExprs(
    "dump-klee-exprs",
    llvm::cl::desc("Dump KLEE expressions after SMTLIB queries"),
    llvm::cl::init(false));

using namespace souper;
using namespace klee;
using namespace llvm;

void CandidateMapEntry::print(llvm::raw_ostream &OS) const {
  std::set<std::string> Functions;

  OS << "; Priority: " << Priority << '\n';

  for (Instruction *O : Origins) {
    std::string FunctionName;
    const Function *F = O->getParent()->getParent();
    if (F->hasLocalLinkage()) {
      FunctionName =
          (F->getParent()->getModuleIdentifier() + ":" + F->getName()).str();
    } else {
      FunctionName = F->getName();
    }
    Functions.insert(FunctionName);
  }

  for (auto F : Functions) {
    OS << "; Function: " << F << '\n';
  }

  PrintReplacement(OS, PCs, Mapping);
}

void souper::AddToCandidateMap(CandidateMap &M,
                               const CandidateReplacement &CR) {
  CandidateExpr CE = GetCandidateExprForReplacement(CR);
  if (!IsTriviallyInvalid(CE.E)) {
    std::string InstStr;
    llvm::raw_string_ostream InstSS(InstStr);
    PrintReplacement(InstSS, CR.PCs, CR.Mapping);

    CandidateMapEntry &Entry = M[InstSS.str()];
    if (Entry.Query.empty()) {
      std::ostringstream SMTSS;
      ConstraintManager Manager;
      Query KQuery(Manager, CE.E);
      ExprSMTLIBLetPrinter Printer;
      Printer.setOutput(SMTSS);
      Printer.setQuery(KQuery);
      Printer.generateOutput();

      if (DumpKLEEExprs) {
        SMTSS << "; KLEE expression:\n; ";
        std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SMTSS));
        PP->setForceNoLineBreaks(true);
        PP->scan(CE.E);
        PP->print(CE.E);
        SMTSS << std::endl;
      }

      Entry.Query = SMTSS.str();

      Entry.PCs = CR.PCs;
      Entry.Mapping = CR.Mapping;
    }

    Entry.Origins.push_back(CR.Origin);
    Entry.Priority += CR.Priority;
  }
}
