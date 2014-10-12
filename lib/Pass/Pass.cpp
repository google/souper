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
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include "souper/Tool/CandidateMapUtils.h"

using namespace souper;
using namespace llvm;

namespace {
std::unique_ptr<Solver> S;
unsigned ReplaceCount;

static cl::opt<bool> DebugSouperPass("souper-debug", cl::Hidden,
                                     cl::init(false), cl::desc("Debug Souper"));

static cl::opt<bool> ProfileSouperOpts("souper-profile-opts", cl::init(false),
                                       cl::desc("Profile Souper optimizations"));

static cl::opt<unsigned> FirstReplace("souper-first-opt", cl::Hidden,
    cl::init(0),
    cl::desc("First Souper optimization to perform (default=0)"));

static cl::opt<unsigned> LastReplace("souper-last-opt", cl::Hidden,
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

  void addProfileCode(LLVMContext &C, Module *M, std::string LHS,
                      std::string SrcLoc, BasicBlock::iterator BI) {
    Function *RegisterFunc = M->getFunction("_souper_profile_register");
    if (!RegisterFunc) {
      Type *RegisterArgs[] = {
        PointerType::getInt8PtrTy(C),
        PointerType::getInt8PtrTy(C),
        PointerType::getInt64PtrTy(C),
      };
      FunctionType *RegisterType = FunctionType::get(Type::getVoidTy(C),
                                                     RegisterArgs, false);
      RegisterFunc = Function::Create(RegisterType, Function::ExternalLinkage,
                                      "_souper_profile_register", M);
    }

    Constant *Repl = ConstantDataArray::getString(C, LHS, true);
    Constant *ReplVar = new GlobalVariable(*M, Repl->getType(), true,
        GlobalValue::PrivateLinkage, Repl, "");
    Constant *ReplPtr = ConstantExpr::getPointerCast(ReplVar,
        PointerType::getInt8PtrTy(C));

    Constant *Field = ConstantDataArray::getString(C, "dprofile " + SrcLoc,
                                                   true);
    Constant *FieldVar = new GlobalVariable(*M, Field->getType(), true,
                                            GlobalValue::PrivateLinkage, Field,
                                            "");
    Constant *FieldPtr = ConstantExpr::getPointerCast(FieldVar,
        PointerType::getInt8PtrTy(C));

    Constant *CntVar = new GlobalVariable(*M, Type::getInt64Ty(C), false,
                                          GlobalValue::PrivateLinkage,
                                          ConstantInt::get(C, APInt(64, 0)),
                                          "_souper_profile_cnt");

    FunctionType *CtorType = FunctionType::get(Type::getVoidTy(C), false);
    Function *Ctor = Function::Create(CtorType, GlobalValue::InternalLinkage,
                                      "_souper_profile_ctor", M);

    BasicBlock *BB = BasicBlock::Create(C, "entry", Ctor);
    IRBuilder<> Builder(BB);

    Builder.CreateCall3(RegisterFunc, ReplPtr, FieldPtr, CntVar);
    Builder.CreateRetVoid();

    appendToGlobalCtors(*M, Ctor, 0);

    new AtomicRMWInst(AtomicRMWInst::Add, CntVar,
                      ConstantInt::get(C, APInt(64, 1)), Monotonic, CrossThread,
                      BI);
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

    for (auto &Cand : CandMap) {
      if (std::error_code EC =
              S->infer(Cand.PCs, Cand.Mapping.LHS, Cand.Mapping.RHS,
                       &Cand.Origin, IC)) {
        if (EC == std::errc::timed_out) {
          continue;
        } else {
          report_fatal_error("Unable to query solver: " + EC.message() + "\n");
        }
      }
      if (!Cand.Mapping.RHS)
        continue;
      Instruction *I = Cand.Origin.getInstruction();

      Constant *CI = ConstantInt::get(I->getType(), Cand.Mapping.RHS->Val);
      if (DebugSouperPass) {
        errs() << "\n";
        errs() << "; Replacing \"";
        I->print(errs());
        errs() << "\"\n; from \"";
        I->getDebugLoc().print(I->getContext(), errs());
        errs() << "\"\n; with \"";
        CI->print(errs());
        errs() << "\" in:\n";
        PrintReplacement(errs(), Cand.PCs, Cand.Mapping);
      }
      if (ReplaceCount >= FirstReplace && ReplaceCount <= LastReplace) {
        BasicBlock::iterator BI = I;
        ReplaceInstWithValue(I->getParent()->getInstList(), BI, CI);
        if (ProfileSouperOpts) {
          std::string Str;
          llvm::raw_string_ostream Loc(Str);
          I->getDebugLoc().print(I->getContext(), Loc);
          addProfileCode (F.getContext(), F.getParent(),
                          GetReplacementLHSString(Cand.PCs, Cand.Mapping.LHS),
                          Loc.str(), BI);
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
