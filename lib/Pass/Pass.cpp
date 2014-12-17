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

#define DEBUG_TYPE "souper"

#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
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
#include "souper/KVStore/KVStore.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include "souper/Tool/CandidateMapUtils.h"

using namespace souper;
using namespace llvm;

STATISTIC(SynthFail, "Failure to synthesize");
STATISTIC(Opts, "Optimizations");

namespace {
std::unique_ptr<Solver> S;
unsigned ReplaceCount;
KVStore *KV;

static cl::opt<bool> DebugSouperPass("souper-debug", cl::Hidden,
                                     cl::init(false), cl::desc("Debug Souper"));

static cl::opt<unsigned> DebugLevel("souper-debug-level", cl::Hidden,
     cl::init(1),
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."));

static cl::opt<bool> DynamicProfile("souper-dynamic-profile", cl::init(false),
    cl::desc("Dynamic profiling of Souper optimizations (default=false)"));

static cl::opt<bool> StaticProfile("souper-static-profile", cl::init(false),
    cl::desc("Static profiling of Souper optimizations (default=false)"));

static cl::opt<bool> IgnoreSolverErrors("souper-ignore-solver-errors",
                                        cl::init(false),
                                        cl::desc("Ignore solver errors"));

static cl::opt<unsigned> FirstReplace("souper-first-opt", cl::Hidden,
    cl::init(0),
    cl::desc("First Souper optimization to perform (default=0)"));

static cl::opt<unsigned> LastReplace("souper-last-opt", cl::Hidden,
    cl::init(std::numeric_limits<unsigned>::max()),
    cl::desc("Last Souper optimization to perform (default=infinite)"));

static bool dumpAllReplacements() {
  return DebugSouperPass && (DebugLevel > 1);
}

struct SouperPass : public FunctionPass {
  static char ID;

private:
  void dynamicProfile(LLVMContext &C, Module *M, std::string LHS,
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

  Value *getValue(Inst *I, Instruction *ReplacedInst,
                  ExprBuilderContext &EBC, PostDominatorTree *DT) {
    Type *T = Type::getIntNTy(ReplacedInst->getContext(), I->Width);
    if (I->K == Inst::Const) {
      return ConstantInt::get(T, I->Val);
    } else if (EBC.Origins.count(I) != 0) {
      // if there's an Origin, we're connecting to existing code
      auto It = EBC.Origins.equal_range(I);
      for (auto O = It.first; O != It.second; ++O) {
        Value *V = O->second;
        if (V->getType() != T)
          continue;
        if (Instruction *IP = dyn_cast<Instruction>(V)) {
          if (DT->dominates(IP->getParent(), ReplacedInst->getParent()))
            return IP;
        } else if (dyn_cast<Argument>(V) || dyn_cast<Constant>(V)) {
          return V;
        } else {
          report_fatal_error("Unhandled LLVM instruction in getValue()");
        }
      }
      return 0;
    } else {
      // otherwise, recursively synthesize
      Value *V1 = getValue(I->Ops[0], ReplacedInst, EBC, DT);
      if (!V1)
        return 0;
      switch (I->K) {
      case Inst::Trunc:
        return new TruncInst(V1, T);
      case Inst::Xor: {
        Value *V2 = getValue(I->Ops[1], ReplacedInst, EBC, DT);
        if (!V2)
          return 0;
        return BinaryOperator::CreateXor(V1, V2);
      }
      case Inst::Sub: {
        Value *V2 = getValue(I->Ops[1], ReplacedInst, EBC, DT);
        if (!V2)
          return 0;
        return BinaryOperator::CreateSub(V1, V2);
      }
      default:
        report_fatal_error((std::string)"Unhandled Souper instruction " +
                           Inst::getKindName(I->K) + " in getValue()");
      }
    }
  }

public:
  void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<PostDominatorTree>();
    Info.addRequired<LoopInfo>();
    // TODO there should be some setPreserves that we can add here
  }

  SouperPass() : FunctionPass(ID) {
    if (!S) {
      S = GetSolverFromArgs(KV);
      if (StaticProfile && !KV)
        KV = new KVStore;
      if (!S)
        report_fatal_error("Souper requires a solver to be specified");
    }
  }

  bool runOnFunction(Function &F) {
    bool changed = false;
    InstContext IC;
    ExprBuilderContext EBC;
    CandidateMap CandMap;

    LoopInfo *LI = &getAnalysis<LoopInfo>();
    PostDominatorTree *DT = &getAnalysis<PostDominatorTree>();

    FunctionCandidateSet CS = ExtractCandidatesFromPass(&F, LI, IC, EBC);

    std::string FunctionName;
    if (F.hasLocalLinkage()) {
      FunctionName =
        (F.getParent()->getModuleIdentifier() + ":" + F.getName()).str();
    } else {
      FunctionName = F.getName();
    }

    if (dumpAllReplacements()) {
      errs() << "\n";
      errs() << "; Listing all replacements for " << FunctionName << "\n";
    }

    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        if (dumpAllReplacements()) {
          Instruction *I = R.Origin.getInstruction();
          errs() << "\n; *****";
          errs() << "\n; For LLVM instruction:\n;";
          I->print(errs());
          errs() << "\n; Generating replacement:\n";
          ReplacementContext Context;
          PrintReplacementLHS(errs(), R.PCs, R.Mapping.LHS, Context);
        }
        AddToCandidateMap(CandMap, R);
      }
    }

    if (DebugSouperPass && 0) {
      errs() << "\n";
      errs() << "; Listing applied replacements for " << FunctionName << "\n";
      errs() << "; Using solver: " << S->getName() << '\n';
    }

    for (auto &Cand : CandMap) {
      if (StaticProfile) {
        std::string Str;
        llvm::raw_string_ostream Loc(Str);
        Instruction *I = Cand.Origin.getInstruction();
        I->getDebugLoc().print(I->getContext(), Loc);
        std::string HField = "sprofile " + Loc.str();
        ReplacementContext Context;
        KV->hIncrBy(GetReplacementLHSString(Cand.PCs, Cand.Mapping.LHS, Context),
                    HField, 1);
      }
      if (std::error_code EC =
          S->infer(Cand.PCs, Cand.Mapping.LHS, Cand.Mapping.RHS, IC)) {
        if (EC == std::errc::timed_out)
          continue;
        if (IgnoreSolverErrors) {
          llvm::errs() << "Unable to query solver: " + EC.message() + "\n";
          continue;
        } else {
          report_fatal_error("Unable to query solver: " + EC.message() + "\n");
        }
      }
      if (!Cand.Mapping.RHS)
        continue;

      Instruction *ReplacedInst = Cand.Origin.getInstruction();
      if (DebugSouperPass) {
        errs() << "\n";
        errs() << "; In:\n";
        PrintReplacement(errs(), Cand.PCs, Cand.Mapping);
        errs() << "; Replacing \"";
        ReplacedInst->print(errs());
        errs() << "\"\n; from \"";
        ReplacedInst->getDebugLoc().print(ReplacedInst->getContext(), errs());
      }
      Value *NewVal = getValue(Cand.Mapping.RHS, ReplacedInst, EBC, DT);
      if (!NewVal) {
        if (DebugSouperPass)
          errs() << "\"\n; replacement failed\n";
        ++SynthFail;
        continue;
      }
      ++Opts;
      if (DebugSouperPass) {
        errs() << "\"\n; with \"";
        NewVal->print(errs());
        errs() << "\"\n";
      }
      EBC.Origins.erase(Cand.Mapping.LHS);
      EBC.Origins.insert(std::pair<Inst *, Value *>(Cand.Mapping.LHS, NewVal));

      if (ReplaceCount >= FirstReplace && ReplaceCount <= LastReplace) {
        BasicBlock::iterator BI = ReplacedInst;
        ReplaceInstWithValue(ReplacedInst->getParent()->getInstList(), BI,
                             NewVal);
        if (DynamicProfile) {
          std::string Str;
          llvm::raw_string_ostream Loc(Str);
          ReplacedInst->getDebugLoc().print(ReplacedInst->getContext(), Loc);
          ReplacementContext Context;
          dynamicProfile (F.getContext(), F.getParent(),
                          GetReplacementLHSString(Cand.PCs, Cand.Mapping.LHS,
                                                  Context), Loc.str(), BI);
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
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
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
