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
#include "souper/Util/DfaUtils.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/KVStore/KVStore.h"
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
                       Solver *S, InstContext &IC, KVStore *KVForStaticProfile) {
  if (S) {
    OS << "; Listing valid replacements.\n";
    OS << "; Using solver: " << S->getName() << '\n';

    std::vector<int> Profile;
    std::map<std::string,int> Index;
    for (int I=0; I < M.size(); ++I) {
      auto &Cand = M[I];
      ReplacementContext Context;
      auto S = GetReplacementLHSString(Cand.BPCs, Cand.PCs,
                                       Cand.Mapping.LHS, Context);
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

      if (KVForStaticProfile) {
        std::string Str;
        llvm::raw_string_ostream Loc(Str);
        Instruction *I = Cand.Origin;
        I->getDebugLoc().print(Loc);
        std::string HField = "sprofile " + Loc.str();
        ReplacementContext Context;
        KVForStaticProfile->hIncrBy(GetReplacementLHSString(Cand.BPCs,
            Cand.PCs, Cand.Mapping.LHS, Context), HField, 1);
      }

      if (isInferDFA()) {
        OS << '\n';
        Cand.printFunction(OS);
        ReplacementContext Context;
        Cand.printLHS(OS, Context);

        if (InferNeg) {
          bool Negative;
          if (std::error_code EC = S->negative(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                                               Negative, IC)) {
            llvm::errs() << "Error: " << EC.message() << '\n';
            return false;
          } else {
            OS << "; negative from souper: "
               << convertToStr(Negative) << "\n";
          }
        }
        if (InferNonNeg) {
          bool NonNegative;
          if (std::error_code EC = S->nonNegative(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                                                  NonNegative, IC)) {
            llvm::errs() << "Error: " << EC.message() << '\n';
            return false;
          } else {
            OS << "; nonNegative from souper: "
               << convertToStr(NonNegative) << "\n";
          }
        }
        if (InferKnownBits) {
          unsigned W = Cand.Mapping.LHS->Width;
          KnownBits Known(W);
          if (std::error_code EC = S->knownBits(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                                                Known, IC)) {
            llvm::errs() << "Error: " << EC.message() << '\n';
            return false;
          } else {
            OS << "; knownBits from souper: "
               << Inst::getKnownBitsString(Known.Zero, Known.One) << "\n";
          }
        }
        if (InferPowerTwo) {
          bool PowTwo;
          if (std::error_code EC = S->powerTwo(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                                               PowTwo, IC)) {
            llvm::errs() << "Error: " << EC.message() << '\n';
            return false;
          } else {
            OS << "; powerOfTwo from souper: "
               << convertToStr(PowTwo) << "\n";
          }
        }
        if (InferNonZero) {
          bool NonZero;
          if (std::error_code EC = S->nonZero(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                                              NonZero, IC)) {
            llvm::errs() << "Error: " << EC.message() << '\n';
            return false;
          } else {
            OS << "; nonZero from souper: "
               << convertToStr(NonZero) << "\n";
          }
        }
        if (InferSignBits) {
          unsigned SignBits;
          if (std::error_code EC = S->signBits(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                                               SignBits, IC)) {
            llvm::errs() << "Error: " << EC.message() << '\n';
            return false;
          } else {
            OS << "; signBits from souper: "
               << std::to_string(SignBits) << "\n";
          }
        }
        if (InferRange) {
          unsigned W = Cand.Mapping.LHS->Width;
          llvm::ConstantRange Range = S->constantRange(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS, IC);

          OS << "; range from souper: " << "[" << Range.getLower()
             << "," << Range.getUpper() << ")" << "\n";
        }
        if (InferDemandedBits) {
          llvm::errs() << "Error: Not Implemented\n";
          return false;
        }
      } else {
        std::vector<Inst *> RHSs;
        if (std::error_code EC =
            S->infer(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                     RHSs, /*AllowMultipleRHSs=*/false, IC)) {
          llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
          return false;
        }

        if (!RHSs.empty()) {
          OS << '\n';
          OS << "; Static profile " << Profile[I] << '\n';
          // use the first RHS in list if there are multiple valid RHSs
          Cand.Mapping.RHS = RHSs.front();
          Cand.printFunction(OS);
          Cand.print(OS);
        }
      }
    }
  } else {
    OS << "; No solver specified; listing all candidate replacements.\n";
    for (auto &Cand : M) {
      OS << '\n';
      Cand.printFunction(OS);
      ReplacementContext Context;
      Cand.printLHS(OS, Context);
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
    std::vector<Inst *> RHSs;
    if (std::error_code EC =
        S->infer(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                 RHSs, /*AllowMultipleRHSs=*/false, IC)) {
      llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
      return false;
    }
    if (!RHSs.empty()) {
       // use the first RHS in list if there are multiple valid RHSs
      Cand.Mapping.RHS = RHSs.front();
      if (Cand.Mapping.RHS->K != Inst::Const) {
        llvm::errs() << "found replacement:\n";
        Cand.printFunction(llvm::errs());
        Cand.print(llvm::errs());
        llvm::errs() << "but cannot yet analyze non-constant replacements\n";
        OK = false;
        continue;
      }
      llvm::APInt ActualVal = Cand.Mapping.RHS->Val;

      llvm::Instruction *Inst = Cand.Origin;
      llvm::MDNode *ExpectedMD = Inst->getMetadata(ExpectedID);
      if (!ExpectedMD) {
        llvm::errs() << "instruction:\n";
        Inst->print(llvm::errs(), /*IsForDebug=*/true);
        llvm::errs() << "\n";
        llvm::errs() << "unexpected simplification:\n";
        Cand.printFunction(llvm::errs());
        Cand.print(llvm::errs());
        OK = false;
        continue;
      }

      if (ExpectedMD->getNumOperands() != 1 ||
          !mdconst::hasa<ConstantInt>(ExpectedMD->getOperand(0))) {
        llvm::errs() << "instruction:\n";
        Inst->print(llvm::errs(), /*IsForDebug=*/true);
        llvm::errs() << "\n";
        llvm::errs() << "invalid metadata\n";
        OK = false;
        continue;
      }
      llvm::APInt ExpectedVal =
        mdconst::extract<ConstantInt>(ExpectedMD->getOperand(0))->getValue();
      Inst->setMetadata(ExpectedID, 0);
      if (ExpectedVal.getBitWidth() != ActualVal.getBitWidth()) {
        llvm::errs() << "metadata width doesn't match value width\n";
        OK = false;
        continue;
      }
      if (ExpectedVal != ActualVal) {
        llvm::errs() << "instruction:\n";
        Inst->print(llvm::errs(), /*IsForDebug=*/true);
        llvm::errs() << "\n";
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
          Inst.print(llvm::errs(), /*IsForDebug=*/true);
          llvm::errs() << "\n";
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
