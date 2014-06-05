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

#include "souper/Tool/CandidateMapUtils.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/CandidateMap.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Extractor/Solver.h"

void souper::AddModuleToCandidateMap(InstContext &IC, ExprBuilderContext &EBC,
                                     CandidateMap &CandMap, llvm::Module *M) {
  for (auto &F : *M) {
    FunctionCandidateSet CS = ExtractCandidates(&F, IC, EBC);
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        AddToCandidateMap(CandMap, R);
      }
    }
  }
}

namespace souper {

bool SolveCandidateMap(llvm::raw_ostream &OS, const CandidateMap &M,
		       Solver *S) {
  if (S) {
    OS << "; Listing valid replacements.\n";
    OS << "; Using solver: " << S->getName() << '\n';
    for (const auto &Cand : M) {
      bool Valid;
#if 1 // FIXME
      Valid = true;
#else
      if (llvm::error_code EC =
              S->isSatisfiable(Cand.second.getQuery(), Sat)) {
        llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
        return false;
      }
#endif
      if (Valid) {
        OS << '\n';
        Cand.second.print(OS);
      }
    }
  } else {
    OS << "; No solver specified; listing all candidate replacements.\n";
    for (const auto &Cand : M) {
      OS << '\n';
      Cand.second.print(OS);
    }
  }

  return true;
}

}
