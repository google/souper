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

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/SMTLIB2/Solver.h"

void souper::AddToCandidateMap(CandidateMap &M,
                               const CandidateReplacement &CR) {
  M.emplace_back(CR);
}

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

bool SolveCandidateMap(llvm::raw_ostream &OS, CandidateMap &M,
                       Solver *S, InstContext &IC) {
  if (S) {
    OS << "; Listing valid replacements.\n";
    OS << "; Using solver: " << S->getName() << '\n';

    std::vector<int> Profile;
    std::map<std::string,int> Index;
    for (int I=0; I < M.size(); ++I) {
      auto &Cand = M[I];
      auto S = GetReplacementLHSString(Cand.PCs, Cand.Mapping.LHS);
      if (Index.find(S) == Index.end()) {
        Index[S] = I;
        Profile.push_back(1);
      } else {
        ++Profile[Index[S]];
        Profile.push_back(0);
      }
    }

    for (int I=0; I < M.size(); ++I) {
      if (Profile[I] == 0)
        continue;
      auto &Cand = M[I];
      Inst *RHS;
      if (std::error_code EC =
              S->infer(Cand.PCs, Cand.Mapping.LHS, RHS, &Cand.Origin, IC)) {
        llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
        return false;
      }
      if (RHS) {
        OS << '\n';
        OS << "; Static profile " << Profile[I] << '\n';
        Cand.Mapping.RHS = RHS;
        Cand.printFunction(OS);
        Cand.print(OS);
      }
    }
  } else {
    OS << "; No solver specified; listing all candidate replacements.\n";
    for (auto &Cand : M) {
      OS << '\n';
      Cand.printFunction(OS);
      Cand.printLHS(OS);
    }
  }

  return true;
}

bool CheckCandidateMap(llvm::Module &Mod, CandidateMap &M, Solver *S,
                       InstContext &IC) {
  if (!S) {
    llvm::errs() << "Solver required in -check mode\n";
    return false;
  }

  unsigned ExpectedID = Mod.getContext().getMDKindID("expected");

  bool OK = true;
  for (auto &Cand : M) {
    Inst *RHS;
    if (std::error_code EC =
            S->infer(Cand.PCs, Cand.Mapping.LHS, RHS, &Cand.Origin, IC)) {
      llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
      return false;
    }
    if (RHS) {
      Cand.Mapping.RHS = RHS;
      if (Cand.Mapping.RHS->K != Inst::Const) {
        llvm::errs() << "found replacement:\n";
        Cand.printFunction(llvm::errs());
        Cand.print(llvm::errs());
        llvm::errs() << "but cannot yet analyze non-constant replacements\n";
        OK = false;
        continue;
      }
      llvm::APInt ActualVal = Cand.Mapping.RHS->Val;

      llvm::Instruction *Inst = Cand.Origin.getInstruction();
      llvm::MDNode *ExpectedMD = Inst->getMetadata(ExpectedID);
      if (!ExpectedMD) {
        llvm::errs() << "instruction:\n";
        Inst->dump();
        llvm::errs() << "unexpected simplification:\n";
        Cand.printFunction(llvm::errs());
        Cand.print(llvm::errs());
        OK = false;
        continue;
      }
      if (ExpectedMD->getNumOperands() != 1 ||
          !isa<llvm::ConstantInt>(ExpectedMD->getOperand(0))) {
        llvm::errs() << "instruction:\n";
        Inst->dump();
        llvm::errs() << "invalid metadata\n";
        OK = false;
        continue;
      }
      llvm::APInt ExpectedVal =
        cast<llvm::ConstantInt>(ExpectedMD->getOperand(0))->getValue();
      Inst->setMetadata(ExpectedID, 0);
      if (ExpectedVal != ActualVal) {
        llvm::errs() << "instruction:\n";
        Inst->dump();
        llvm::errs() << "unexpected simplification, wanted " << ExpectedVal
                     << ":\n";
        Cand.printFunction(llvm::errs());
        Cand.print(llvm::errs());
        OK = false;
        continue;
      }
    }
  }

  for (const auto &F : Mod) {
    for (const auto &BB : F) {
      for (const auto &Inst : BB) {
        llvm::MDNode *ExpectedMD = Inst.getMetadata(ExpectedID);
        if (ExpectedMD) {
          llvm::errs() << "instruction:\n";
          Inst.dump();
          llvm::errs() << "expected simplification, none found\n";
          OK = false;
          continue;
        }
      }
    }
  }

  return OK;
}

}
