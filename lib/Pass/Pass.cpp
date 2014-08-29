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

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include "souper/Tool/CandidateMapUtils.h"

using namespace souper;
using namespace llvm;

namespace {
std::unique_ptr<Solver> S;
unsigned ReplaceCount;

static cl::opt<bool> DebugSouperPass("debug-souper", cl::Hidden,
                                     cl::init(false), cl::desc("Debug Souper"));

static cl::opt<bool> ProfileSouperOpts("profile-souper-opts", cl::Hidden,
                                       cl::init(false),
                                       cl::desc("Profile Souper optimizations"));

static cl::opt<unsigned> FirstReplace("first-souper-opt", cl::Hidden,
    cl::init(0),
    cl::desc("First Souper optimization to perform (default=0)"));

static cl::opt<unsigned> LastReplace("last-souper-opt", cl::Hidden,
    cl::init(std::numeric_limits<unsigned>::max()),
    cl::desc("Last Souper optimization to perform (default=infinite)"));

struct SouperPass : public FunctionPass {
  static char ID;

public:
  SouperPass() : FunctionPass(ID) {
    if (!S) {
      S = GetSolverFromArgs();
      if (!S)
        report_fatal_error("Souper requires a solver to be specified");
    }
  }

  void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<LoopInfo>();
  }

  bool runOnFunction(Function &F) {
    bool changed = false;
    InstContext IC;
    ExprBuilderContext EBC;
    CandidateMap CandMap;

    LoopInfo *LI = &getAnalysis<LoopInfo>();

    FunctionCandidateSet CS = ExtractCandidatesFromPass(&F, LI, IC, EBC);
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        AddToCandidateMap(CandMap, R);
      }
    }

    if (DebugSouperPass) {
      std::string FunctionName;
      if (F.hasLocalLinkage()) {
        FunctionName =
            (F.getParent()->getModuleIdentifier() + ":" + F.getName()).str();
      } else {
        FunctionName = F.getName();
      }
      errs() << "\n";
      errs() << "; Listing applied replacements for " << FunctionName << "\n";
      errs() << "; Using solver: " << S->getName() << '\n';
    }

    DenseSet<Instruction *> ReplacedInsts;

    for (const auto &Cand : CandMap) {
      bool Valid;
      if (std::error_code EC =
              S->isValid(Cand.second.PCs, Cand.second.Mapping, Valid, 0)) {
        if (EC == std::errc::timed_out) {
          continue;
        } else {
          report_fatal_error("Unable to query solver: " + EC.message() + "\n");
        }
      }

      if (!Valid)
        continue;

      for (auto *O : Cand.second.Origins) {
        if (!ReplacedInsts.insert(O).second)
          continue;

        Constant *CI = ConstantInt::get(
            O->getType(), Cand.second.Mapping.Replacement->Val);
        if (DebugSouperPass) {
          errs() << "\n";
          errs() << "; Priority: " << Cand.second.Priority << '\n';
          errs() << "; Replacing \"";
          O->print(errs());
          errs() << "\"\n; from \"";
          O->getDebugLoc().print(O->getContext(), errs());
          errs() << "\"\n; with \"";
          CI->print(errs());
          errs() << "\" in:\n";
          PrintReplacement(errs(), Cand.second.PCs, Cand.second.Mapping);
        }
        if (ReplaceCount >= FirstReplace && ReplaceCount <= LastReplace) {
          BasicBlock::iterator BI = O;
          ReplaceInstWithValue(O->getParent()->getInstList(), BI, CI);
          if (ProfileSouperOpts) {
            Module &M = *F.getParent();
            LLVMContext &C = F.getContext();
            std::vector<Type*> FA;
            FA.push_back(PointerType::getInt8PtrTy(C));
            FunctionType *FT = FunctionType::get(Type::getVoidTy(C),
                                                 FA, false);
            Function *Handler = Function::Create(FT, Function::ExternalLinkage,
                                                 "_xxx_", &M);
            Constant *S = ConstantDataArray::getString(C, "Hello");
            Constant *SV = new GlobalVariable (M, S->getType(), true,
                                            GlobalValue::PrivateLinkage, S, "");
            Constant *Cast = ConstantExpr::getPointerCast(SV, PointerType::getInt8PtrTy(C));
            std::vector<Value*> Args;
            Args.push_back (Cast);
            CallInst::Create(Handler, Args, "", BI);
          }
          changed = true;
        } else {
          if (DebugSouperPass)
            errs() << "Skipping this replacement (number " << ReplaceCount <<
              ")\n";
        }
        if (ReplaceCount < std::numeric_limits<unsigned>::max())
          ++ReplaceCount;
      }
    }

    return changed;
  }
};

char SouperPass::ID = 0;
}

namespace llvm {
void initializeSouperPassPass(llvm::PassRegistry &);
}

INITIALIZE_PASS_BEGIN(SouperPass, "souper", "Souper super-optimizer pass",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_END(SouperPass, "souper", "Souper super-optimizer pass", false,
                    false)

static struct Register {
  Register() {
    initializeSouperPassPass(*llvm::PassRegistry::getPassRegistry());
  }
} X;

static void registerSouperPass(
    const llvm::PassManagerBuilder &Builder, llvm::PassManagerBase &PM) {
  PM.add(new SouperPass);
}

static llvm::RegisterStandardPasses
RegisterSouperOptimizer(llvm::PassManagerBuilder::EP_Peephole,
                        registerSouperPass);
