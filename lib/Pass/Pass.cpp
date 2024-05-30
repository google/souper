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

#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/DemandedBits.h"
#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/UnreachableBlockElim.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Scalar/ADCE.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "souper/KVStore/KVStore.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Codegen/Codegen.h"
#include "souper/Tool/GetSolver.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "set"

#define DEBUG_TYPE "souper"
STATISTIC(InstructionReplaced, "Number of instructions replaced by another instruction");
STATISTIC(DominanceCheckFailed, "Number of failed replacement due to dominance check");

using namespace souper;
using namespace llvm;

unsigned DebugLevel;

namespace {
std::unique_ptr<Solver> S;
unsigned ReplacementIdx, ReplacementsDone, LHSNum;
KVStore *KV;

static cl::opt<unsigned, /*ExternalStorage=*/true>
DebugFlagParser("souper-debug-level",
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."),
     cl::location(DebugLevel), cl::init(1));

static cl::opt<bool> Verify("souper-verify", cl::init(false),
    cl::desc("Verify functions before Souper processes them (default=false)"));

static cl::opt<bool> DynamicProfile("souper-dynamic-profile", cl::init(false),
    cl::desc("Dynamic profiling of Souper optimizations (default=false)"));

static cl::opt<bool> StaticProfile("souper-static-profile", cl::init(false),
    cl::desc("Static profiling of Souper optimizations (default=false)"));

static cl::opt<unsigned> FirstReplace("souper-first-opt", cl::Hidden,
    cl::init(0),
    cl::desc("First Souper optimization to perform (default=0)"));

static cl::opt<unsigned> LastReplace("souper-last-opt", cl::Hidden,
    cl::init(std::numeric_limits<unsigned>::max()),
    cl::desc("Last Souper optimization to perform (default=infinite)"));

#ifdef DYNAMIC_PROFILE_ALL
static const bool DynamicProfileAll = true;
#else
static const bool DynamicProfileAll = false;
#endif

static void eliminateDeadCode(Function &F) {
  FunctionPassManager FPM;
  FPM.addPass(DCEPass());
  FunctionAnalysisManager FAM;
  FAM.registerPass([&] { return TargetLibraryAnalysis(); });
  FAM.registerPass([&] { return PassInstrumentationAnalysis(); });
  FPM.run(F, FAM);
}

struct SouperPass : PassInfoMixin<SouperPass> {
  static char ID;

  Value* getOperand(Inst* I, unsigned index, Instruction *ReplacedInst,
                    ExprBuilderContext &EBC, DominatorTree &DT,
                    std::map<Inst *, Value *> &ReplacedValues,
                    IRBuilder<> &Builder, Module *M) {
    Value *Result = nullptr;
    if (Inst::isOverflowIntrinsicMain(I->K)) {
      assert(I->Ops.size() == 2 && I->Ops[0]->Ops.size() == 2);
      Result = getValue(I->Ops[0]->Ops[index], ReplacedInst, EBC, DT, ReplacedValues, Builder, M);
    } else {
      Result = getValue(I->Ops[index], ReplacedInst, EBC, DT, ReplacedValues, Builder, M);
    }

    return Result;
  }

public:
  void dynamicProfile(Function *F, CandidateReplacement &Cand) {
    std::string Str;
    llvm::raw_string_ostream Loc(Str);
    Cand.Origin->getDebugLoc().print(Loc);
    ReplacementContext Context;
    std::string LHS = GetReplacementLHSString(Cand.BPCs, Cand.PCs,
                                              Cand.Mapping.LHS, Context);
    LLVMContext &C = F->getContext();
    Module *M = F->getParent();
    Function *RegisterFunc = M->getFunction("_souper_profile_register");
    if (!RegisterFunc) {
      Type *RegisterArgs[] = {
        PointerType::get(Type::getInt8Ty(C), 0),
        PointerType::get(Type::getInt8Ty(C), 0),
        PointerType::get(Type::getInt64Ty(C), 0),
      };
      FunctionType *RegisterType = FunctionType::get(Type::getVoidTy(C),
                                                     RegisterArgs, false);
      RegisterFunc = Function::Create(RegisterType, Function::ExternalLinkage,
                                      "_souper_profile_register", M);
    }

    // todo: should check if this string exists before creating it
    Constant *Repl = ConstantDataArray::getString(C, LHS, true);
    Constant *ReplVar = new GlobalVariable(*M, Repl->getType(), true,
        GlobalValue::PrivateLinkage, Repl, "");
    Constant *ReplPtr = ConstantExpr::getPointerCast(ReplVar,
        PointerType::get(Type::getInt8Ty(C), 0));

    Constant *Field = ConstantDataArray::getString(C, "dprofile " + Loc.str(),
                                                   true);
    Constant *FieldVar = new GlobalVariable(*M, Field->getType(), true,
                                            GlobalValue::PrivateLinkage, Field,
                                            "");
    Constant *FieldPtr = ConstantExpr::getPointerCast(FieldVar,
        PointerType::get(Type::getInt8Ty(C), 0));

    Constant *CntVar = new GlobalVariable(*M, Type::getInt64Ty(C), false,
                                          GlobalValue::PrivateLinkage,
                                          ConstantInt::get(C, APInt(64, 0)),
                                          "_souper_profile_cnt");

    FunctionType *CtorType = FunctionType::get(Type::getVoidTy(C), false);
    Function *Ctor = Function::Create(CtorType, GlobalValue::InternalLinkage,
                                      "_souper_profile_ctor", M);

    BasicBlock *BB = BasicBlock::Create(C, "entry", Ctor);
    IRBuilder<> Builder(BB);

    Value *Args[] = { ReplPtr, FieldPtr, CntVar };
    Builder.CreateCall(RegisterFunc, Args);
    Builder.CreateRetVoid();

    appendToGlobalCtors(*M, Ctor, 0);

    BasicBlock::iterator BI(Cand.Origin);
    while (isa<PHINode>(*BI))
      ++BI;
    Align A{8};
    new AtomicRMWInst(AtomicRMWInst::Add, CntVar,
                      ConstantInt::get(C, APInt(64, 1)),
                      A, AtomicOrdering::Monotonic,
                      SyncScope::System, Cand.Origin);
  }

  Value *getValue(Inst *I, Instruction *ReplacedInst,
                  ExprBuilderContext &EBC, DominatorTree &DT,
                  std::map<Inst *, Value *> &ReplacedValues,
                  IRBuilder<> &Builder, Module *M) {
    return Codegen(ReplacedInst->getContext(), M, Builder, &DT, ReplacedInst,
                   ReplacedValues)
        .getValue(I);
  }

  bool runOnFunction(Function &F, FunctionAnalysisManager &FAM) {
    std::string FunctionName;
    if (F.hasLocalLinkage()) {
      FunctionName =
        (F.getParent()->getModuleIdentifier() + ":" + F.getName()).str();
    } else {
      FunctionName = F.getName();
    }

    if (DebugLevel > 1) {
      errs() << "\n";
      errs() << "; entering Souper's runOnFunction() for " << FunctionName << "()\n\n";
      F.getParent()->dump();
      errs() << "\n";
    }

    auto &LI = FAM.getResult<llvm::LoopAnalysis>(F);
    auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
    auto &DB = FAM.getResult<DemandedBitsAnalysis>(F);
    auto &LVI = FAM.getResult<LazyValueAnalysis>(F);
    auto &SE = FAM.getResult<ScalarEvolutionAnalysis>(F);
    auto &TLI = FAM.getResult<TargetLibraryAnalysis>(F);

    InstContext IC;
    ExprBuilderContext EBC;
    std::map<Inst *, Value *> ReplacedValues;

    // Run UnreachableBlockElim and ADCE locally
    // TODO: In the long run, switch this tool to the new pass manager.
    {
      FunctionPassManager FM2;
      FunctionAnalysisManager FAM2;
      FAM2.registerPass([&] { return PassInstrumentationAnalysis(); });
      FAM2.registerPass([&] { return DominatorTreeAnalysis(); });
      FAM2.registerPass([&] { return PostDominatorTreeAnalysis(); });
      FM2.addPass(UnreachableBlockElimPass());
      FM2.addPass(ADCEPass());
      FM2.run(F, FAM2);
    }

    FunctionCandidateSet CS = ExtractCandidatesFromPass(F, LI, DB, LVI, SE, TLI, IC, EBC);

    if (DebugLevel > 3)
      errs() << "; extracted candidates\n";

    CandidateMap CandMap;
    for (auto &B : CS.Blocks)
      for (auto &R : B->Replacements)
        AddToCandidateMap(CandMap, R);

    for (auto &Cand : CandMap) {

      if (DebugLevel > 1)
        errs() << "\n================= LHS number " << ++LHSNum << " ====================\n\n";

      if (DebugLevel > 3) {
        errs() << "\n; For LLVM instruction:\n;";
        Cand.Origin->print(errs());
        errs() << "\n; Looking for a replacement for:\n";
        ReplacementContext Context;
        PrintReplacementLHS(errs(), Cand.BPCs, Cand.PCs, Cand.Mapping.LHS, Context);
      }
      
      if (StaticProfile) {
        std::string Str;
        llvm::raw_string_ostream Loc(Str);
        Cand.Origin->getDebugLoc().print(Loc);
        std::string HField = "sprofile " + Loc.str();
        ReplacementContext Context;
        KV->hIncrBy(GetReplacementLHSString(Cand.BPCs, Cand.PCs,
                                            Cand.Mapping.LHS,
                                            Context), HField, 1);
      }
      if (DynamicProfileAll) {
        dynamicProfile(&F, Cand);
        continue;
      }
      std::vector<Inst *> RHSs;
      if (std::error_code EC =
          S->infer(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                   RHSs, /*AllowMultipleRHSs=*/false, IC)) {
        if (EC == std::errc::timed_out ||
            EC == std::errc::value_too_large) {
          if (DebugLevel > 1)
            errs() << "query error for LHS number " << LHSNum << "\n";
          continue;
        } else {
          llvm::errs() << "[FIXME: Crash commented out]\nUnable to query solver: " + EC.message() + "\n";
          errs() << "query error for LHS number " << LHSNum << "\n";
          continue;
          // TODO: This is a temporary workaround to suppress a protocol error which is encountered
          // once in SPEC 2017. This workaround does not have a negative effect other than maybe
          // missing one potential transformation.
          //report_fatal_error("Unable to query solver: " + EC.message() + "\n");
        }
      }
      if (RHSs.empty()) {
        if (DebugLevel > 1)
          errs() << "no solutions for LHS number " << LHSNum << "\n";
        continue;
      }

      Cand.Mapping.RHS = RHSs.front();

      Instruction *I = Cand.Origin;
      assert(Cand.Mapping.LHS->K == Inst::Const || Cand.Mapping.LHS->hasOrigin(I));
      IRBuilder<> Builder(I);

      Value *NewVal = getValue(Cand.Mapping.RHS, I, EBC, DT,
                               ReplacedValues, Builder, F.getParent());

      // if LHS comes from use, then NewVal should be a constant
      assert(Cand.Mapping.LHS->HarvestKind != HarvestType::HarvestedFromUse ||
             isa<llvm::Constant>(NewVal));

      // TODO can we assert that getValue() succeeds?
      if (!NewVal) {
        if (DebugLevel > 1)
          errs() << "\"\n; replacement failed for LHS number " << LHSNum << ", getValue() returned null\n";
        continue;
      }

      // here we finally commit to having a viable replacement

      if (ReplacementIdx < FirstReplace || ReplacementIdx > LastReplace) {
        if (DebugLevel > 1)
          errs() << "Skipping this replacement (number " << ReplacementIdx << ")\n";
        if (ReplacementIdx < std::numeric_limits<unsigned>::max())
          ++ReplacementIdx;
        continue;
      }
      if (ReplacementIdx < std::numeric_limits<unsigned>::max())
        ++ReplacementIdx;
      ReplacementsDone++;

      if (Cand.Mapping.LHS->HarvestKind == HarvestType::HarvestedFromDef)
        ReplacedValues[Cand.Mapping.LHS] = NewVal;

      if (DebugLevel > 1) {
        if (DebugLevel > 2) {
          if (DebugLevel > 4) {
            errs() << "\nModule before replacement:\n";
            F.getParent()->dump();
          } else {
            errs() << "\nFunction before replacement:\n";
            F.print(errs());
          }
        }
        errs() << "\n";
        errs() << "; Replacing \"";
        I->print(errs());
        errs() << "\"\n; from \"";
        I->getDebugLoc().print(errs());
        errs() << "\"\n; with \"";
        NewVal->print(errs());
        errs() << "\" in:\n\"";
        PrintReplacement(errs(), Cand.BPCs, Cand.PCs, Cand.Mapping);
        errs() << "\"\n; with \"";
        NewVal->print(errs());
        errs() << "\"\n";
      }

      if (DynamicProfile)
        dynamicProfile(&F, Cand);

      if (Cand.Mapping.LHS->HarvestKind == HarvestType::HarvestedFromDef) {
        I->replaceAllUsesWith(NewVal);
      } else {
        for (llvm::Value::use_iterator UI = I->use_begin();
             UI != I->use_end(); ) {
          llvm::Use &U = *UI;
          ++UI;
          // TODO: Handle general values, not only instructions
          auto *Usr = dyn_cast<llvm::Instruction>(U.getUser());
          if (Usr && Usr->getParent() == Cand.Mapping.LHS->HarvestFrom) {
            U.set(NewVal);
          }
        }
      }

      eliminateDeadCode(F);

      if (DebugLevel > 2) {
        if (DebugLevel > 4) {
          errs() << "\nModule after replacement:\n";
          F.getParent()->dump();
        } else {
          errs() << "\nFunction after replacement:\n\n";
          F.print(errs());
        }
        errs() << "\n";
      }

      if (DebugLevel > 1)
        errs() << "done with LHS number " << LHSNum << " after doing a replacement\n";
      
      return true;
    }

    return false;
  }
  
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
    if (!S) {
      S = GetSolver(KV);
      if (StaticProfile && !KV)
        KV = new KVStore;
    }

    if (Verify && verifyFunction(F))
      llvm::report_fatal_error(("function " + F.getName() + " broken before Souper").str().c_str());

    bool res;
    do {
      res = runOnFunction(F, FAM);
      if (res && verifyFunction(F))
        llvm::report_fatal_error("function broken after Souper changed it");
    } while (res);
    
    return PreservedAnalyses::none();
  }

};

}

namespace llvm {
  void initializeSouperPass(llvm::PassRegistry &);
}

char SouperPass::ID = 0;

bool pipelineParsingCallback(StringRef Name, FunctionPassManager &FPM,
                             ArrayRef<PassBuilder::PipelineElement>) {
  if (Name == "souper") {
    FPM.addPass(SouperPass());
    return true;
  } else {
    return false;
  }
}

void passBuilderCallback(PassBuilder &PB) {
  PB.registerPipelineParsingCallback(pipelineParsingCallback);
  // FIXME: use EP_OptimizerLast to support dynamic profiling
  PB.registerPeepholeEPCallback(
        [](llvm::FunctionPassManager &FPM, llvm::OptimizationLevel Level) {
        FPM.addPass(SouperPass());
      });
}

PassPluginLibraryInfo getSouperPassPluginInfo() {
  llvm::PassPluginLibraryInfo Res;
  Res.APIVersion = LLVM_PLUGIN_API_VERSION;
  Res.PluginName = "souper";
  Res.PluginVersion = LLVM_VERSION_STRING;
  Res.RegisterPassBuilderCallbacks = passBuilderCallback;
  return Res;
}

extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo() {
  return getSouperPassPluginInfo();
}
