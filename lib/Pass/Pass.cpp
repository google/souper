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
#include "llvm/Analysis/DemandedBits.h"
#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "souper/KVStore/KVStore.h"
#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "set"

STATISTIC(InstructionReplaced, "Number of instructions replaced by another instruction");
STATISTIC(DominanceCheckFailed, "Number of failed replacement due to dominance check");

using namespace souper;
using namespace llvm;

namespace {
std::unique_ptr<Solver> S;
unsigned ReplacementIdx, ReplacementsDone;
KVStore *KV;

static cl::opt<unsigned> DebugLevel("souper-debug-level", cl::Hidden,
     cl::init(1),
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."));

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

struct SouperPass : public ModulePass {
  static char ID;

public:
  SouperPass() : ModulePass(ID) {
    if (!S) {
      S = GetSolverFromArgs(KV);
      if (StaticProfile && !KV)
        KV = new KVStore;
      if (!S)
        report_fatal_error("Souper requires a solver to be specified");
    }
  }

  void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<LoopInfoWrapperPass>();
    Info.addRequired<DominatorTreeWrapperPass>();
    Info.addRequired<DemandedBitsWrapperPass>();
    Info.addRequired<LazyValueInfoWrapperPass>();
    Info.addRequired<ScalarEvolutionWrapperPass>();
    Info.addRequired<TargetLibraryInfoWrapperPass>();
  }

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
        PointerType::getInt8PtrTy(C),
        PointerType::getInt8PtrTy(C),
        PointerType::getInt64PtrTy(C),
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
        PointerType::getInt8PtrTy(C));

    Constant *Field = ConstantDataArray::getString(C, "dprofile " + Loc.str(),
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

    Value *Args[] = { ReplPtr, FieldPtr, CntVar };
    Builder.CreateCall(RegisterFunc, Args);
    Builder.CreateRetVoid();

    appendToGlobalCtors(*M, Ctor, 0);

    BasicBlock::iterator BI(Cand.Origin);
    while (isa<PHINode>(*BI))
      ++BI;
    new AtomicRMWInst(AtomicRMWInst::Add, CntVar,
                      ConstantInt::get(C, APInt(64, 1)), AtomicOrdering::Monotonic,
                      SyncScope::System, Cand.Origin);
  }

  Value *getValue(Inst *I, Instruction *ReplacedInst,
                  ExprBuilderContext &EBC, DominatorTree &DT,
                  std::map<Inst *, Value *> &ReplacedValues,
                  IRBuilder<> &Builder, Module *M) {
    Type *T = Type::getIntNTy(ReplacedInst->getContext(), I->Width);
    if (I->K == Inst::Const)
      return ConstantInt::get(T, I->Val);

    if (ReplacedValues.find(I) != ReplacedValues.end())
      return ReplacedValues[I];

    if (I->Origins.size() > 0) {
      // if there's an Origin, we're connecting to existing code
      for (auto V : I->Origins) {
        if (V->getType() != T)
          continue; // TODO: can we assert this doesn't happen?
        if (isa<Argument>(V) || isa<Constant>(V))
          return V;
        if (auto IP = dyn_cast<Instruction>(V)) {
          if (DT.dominates(IP, ReplacedInst)) {
            ++InstructionReplaced;
            return V;
          } else {
            ++DominanceCheckFailed;
          }
        } else {
          report_fatal_error("Unhandled LLVM instruction in getValue()");
        }
      }
      return 0;
    }

    // otherwise, recursively generate code
    Value *V0 = getValue(I->Ops[0], ReplacedInst, EBC, DT, ReplacedValues,
                         Builder, M);
    if (!V0)
      return 0;
    switch (I->Ops.size()) {
    case 1:
      switch (I->K) {
      case Inst::SExt:
        return Builder.CreateSExt(V0, T);
      case Inst::ZExt:
        return Builder.CreateZExt(V0, T);
      case Inst::Trunc:
        return Builder.CreateTrunc(V0, T);
      case Inst::CtPop:{
        Function *F = Intrinsic::getDeclaration(M, Intrinsic::ctpop, T);
        return Builder.CreateCall(F, V0);
      }
      case Inst::BSwap:{
        Function *F = Intrinsic::getDeclaration(M, Intrinsic::bswap, T);
        return Builder.CreateCall(F, V0);
      }
      case Inst::BitReverse:{
	Function *F = Intrinsic::getDeclaration(M, Intrinsic::bitreverse, T);
	return Builder.CreateCall(F, V0);
      }
      case Inst::Cttz:{
        Function *F = Intrinsic::getDeclaration(M, Intrinsic::cttz, T);
        // According to LLVM LangRef, the second argument of cttz i1 <is_zero_undef>
        // must be a constant and is a flag to indicate whether the intrinsic should
        // ensure that a zero as the first argument produces a defined result.
        return Builder.CreateCall(F, {V0,
              ConstantInt::get(V0->getContext(), APInt(1, 0))});
      }
      case Inst::Ctlz:{
        // Ditto
        Function *F = Intrinsic::getDeclaration(M, Intrinsic::ctlz, T);
        return Builder.CreateCall(F, {V0,
              ConstantInt::get(V0->getContext(), APInt(1, 0))});
      }
      default:
        break;
      }
    case 2:{
      Value *V1 = getValue(I->Ops[1], ReplacedInst, EBC, DT,
                           ReplacedValues, Builder, M);
      if (!V1)
        return 0;
      switch (I->K) {
      case Inst::And:
        if (isa<Constant>(V0))
          return Builder.CreateAnd(V1, V0);
        return Builder.CreateAnd(V0, V1);
      case Inst::Or:
        if (isa<Constant>(V0))
          return Builder.CreateOr(V1, V0);
        return Builder.CreateOr(V0, V1);
      case Inst::Xor:
        if (isa<Constant>(V0))
          return Builder.CreateXor(V1, V0);
        return Builder.CreateXor(V0, V1);
      case Inst::Add:
        if (isa<Constant>(V0))
          return Builder.CreateAdd(V1, V0);
        return Builder.CreateAdd(V0, V1);
      case Inst::Sub:
        return Builder.CreateSub(V0, V1);
      case Inst::Mul:
        if (isa<Constant>(V0))
          return Builder.CreateMul(V1, V0);
        return Builder.CreateMul(V0, V1);
      case Inst::Shl:
        return Builder.CreateShl(V0, V1);
      case Inst::AShr:
        return Builder.CreateAShr(V0, V1);
      case Inst::LShr:
        return Builder.CreateLShr(V0, V1);
      case Inst::Ne:
        if (isa<Constant>(V0)) {
          return Builder.CreateICmpNE(V1, V0);
        } else {
          return Builder.CreateICmpNE(V0, V1);
        }
      case Inst::Eq:
        if (isa<Constant>(V0)) {
          return Builder.CreateICmpEQ(V1, V0);
        } else {
          return Builder.CreateICmpEQ(V0, V1);
        }
      case Inst::Ult:
        if (isa<Constant>(V0)) {
          return Builder.CreateICmpUGT(V1, V0);
        } else {
          return Builder.CreateICmpULT(V0, V1);
        }
      case Inst::Slt:
        if (isa<Constant>(V0)) {
          return Builder.CreateICmpSGT(V1, V0);
        } else {
          return Builder.CreateICmpSLT(V0, V1);
        }
      case Inst::Ule:
        if (isa<Constant>(V0)) {
          return Builder.CreateICmpUGE(V1, V0);
        } else {
          return Builder.CreateICmpULE(V0, V1);
        }
      case Inst::Sle:
        if (isa<Constant>(V0)) {
          return Builder.CreateICmpSGE(V1, V0);
        } else {
          return Builder.CreateICmpSLE(V0, V1);
        }
      case Inst::SAddSat:
	return Builder.CreateCall(Intrinsic::getDeclaration(M, Intrinsic::sadd_sat, T), {V0, V1});
      case Inst::UAddSat:
	return Builder.CreateCall(Intrinsic::getDeclaration(M, Intrinsic::uadd_sat, T), {V0, V1});
      case Inst::SSubSat:
	return Builder.CreateCall(Intrinsic::getDeclaration(M, Intrinsic::ssub_sat, T), {V0, V1});
      case Inst::USubSat:
	return Builder.CreateCall(Intrinsic::getDeclaration(M, Intrinsic::usub_sat, T), {V0, V1});
      default:
        break;
      }
    }
    case 3:{
      Value *V1 = getValue(I->Ops[1], ReplacedInst, EBC, DT,
                           ReplacedValues, Builder, M);
      Value *V2 = getValue(I->Ops[2], ReplacedInst, EBC, DT,
                           ReplacedValues, Builder, M);
      if (!V1 || !V2)
        return 0;
      switch (I->K) {
      case Inst::Select:
        return Builder.CreateSelect(V0, V1, V2);
      case Inst::FShl:
      case Inst::FShr: {
        Intrinsic::ID ID =
            I->K == Inst::FShl ? Intrinsic::fshl : Intrinsic::fshr;
        Function *F = Intrinsic::getDeclaration(M, ID, T);
        return Builder.CreateCall(F, {V0, V1, V2});
      }
      default:
        break;
      }
    }
    default:
      break;
    }
    report_fatal_error((std::string)"Unhandled Souper instruction " +
                       Inst::getKindName(I->K) + " in getValue()");
  }

  bool runOnFunction(Function *F) {
    bool Changed = false;
    InstContext IC;
    ExprBuilderContext EBC;
    std::map<Inst *, Value *> ReplacedValues;
    LoopInfo *LI = &getAnalysis<LoopInfoWrapperPass>(*F).getLoopInfo();
    if (!LI)
      report_fatal_error("getLoopInfo() failed");
    auto &DT = getAnalysis<DominatorTreeWrapperPass>(*F).getDomTree();
    DemandedBits *DB = &getAnalysis<DemandedBitsWrapperPass>(*F).getDemandedBits();
    if (!DB)
      report_fatal_error("getDemandedBits() failed");
    LazyValueInfo *LVI = &getAnalysis<LazyValueInfoWrapperPass>(*F).getLVI();
    if (!LVI)
      report_fatal_error("getLVI() failed");
    ScalarEvolution *SE = &getAnalysis<ScalarEvolutionWrapperPass>(*F).getSE();
    if (!SE)
      report_fatal_error("getSE() failed");
    TargetLibraryInfo* TLI = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
    if (!TLI)
      report_fatal_error("getTLI() failed");
    FunctionCandidateSet CS = ExtractCandidatesFromPass(F, LI, DB, LVI, SE, TLI, IC, EBC);

    std::string FunctionName;
    if (F->hasLocalLinkage()) {
      FunctionName =
        (F->getParent()->getModuleIdentifier() + ":" + F->getName()).str();
    } else {
      FunctionName = F->getName();
    }

    if (DebugLevel > 1) {
      errs() << "\n";
      errs() << "; Listing all replacements for " << FunctionName << "\n";
      errs() << "; Using solver: " << S->getName() << '\n';
    }

    CandidateMap CandMap;
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        if (DebugLevel > 3) {
          errs() << "\n; *****";
          errs() << "\n; For LLVM instruction:\n;";
          R.Origin->print(errs());
          errs() << "\n; Generating replacement:\n";
          ReplacementContext Context;
          PrintReplacementLHS(errs(), R.BPCs, R.PCs, R.Mapping.LHS, Context);
        }
        AddToCandidateMap(CandMap, R);
      }
    }


    for (auto &Cand : CandMap) {

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
        dynamicProfile(F, Cand);
        Changed = true;
        continue;
      }
      if (std::error_code EC =
          S->infer(Cand.BPCs, Cand.PCs, Cand.Mapping.LHS,
                   Cand.Mapping.RHS, IC)) {
        if (EC == std::errc::timed_out ||
            EC == std::errc::value_too_large) {
          continue;
        } else {
          report_fatal_error("Unable to query solver: " + EC.message() + "\n");
        }
      }
      if (!Cand.Mapping.RHS)
        continue;

      Instruction *I = Cand.Origin;
      assert(Cand.Mapping.LHS->hasOrigin(I));
      IRBuilder<> Builder(I);

      Value *NewVal = getValue(Cand.Mapping.RHS, I, EBC, DT,
                               ReplacedValues, Builder, F->getParent());

      // if LHS comes from use, then NewVal should be a constant
      assert(Cand.Mapping.LHS->HarvestKind != HarvestType::HarvestedFromUse ||
             isa<llvm::Constant>(NewVal));

      // TODO can we assert that getValue() succeeds?
      if (!NewVal) {
        if (DebugLevel > 1)
          errs() << "\"\n; replacement failed\n";
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
            F->getParent()->dump();
          } else {
            errs() << "\nFunction before replacement:\n";
            F->print(errs());
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
        dynamicProfile(F, Cand);

      if (Cand.Mapping.LHS->HarvestKind == HarvestType::HarvestedFromDef) {
        I->replaceAllUsesWith(NewVal);
        Changed = true;
      } else {
        for (llvm::Value::use_iterator UI = I->use_begin();
             UI != I->use_end(); ) {
          llvm::Use &U = *UI;
          ++UI;
          // TODO: Handle general values, not only instructions
          auto *Usr = dyn_cast<llvm::Instruction>(U.getUser());
          if (Usr && Usr->getParent() == Cand.Mapping.LHS->HarvestFrom) {
            U.set(NewVal);
            Changed = true;
          }
        }
      }
    }

    if (DebugLevel > 2) {
      if (DebugLevel > 4) {
        errs() << "\nModule after replacement:\n";
        F->getParent()->dump();
      } else {
        errs() << "\nFunction after replacement:\n\n";
        F->print(errs());
      }
      errs() << "\n";
    }

    return Changed;
  }

  bool runOnModule(Module &M) {
    bool Changed = false;
    // get the list first since the dynamic profiling adds functions as it goes
    std::vector<Function *> FL;
    for (auto &I : M)
      FL.push_back((Function *)&I);
    for (auto *F : FL)
      if (!F->isDeclaration())
        Changed = runOnFunction(F) || Changed;
    if (DebugLevel > 1)
      errs() << "\nTotal of " << ReplacementsDone << " replacements done on this module\n";
    return Changed;
  }

};

char SouperPass::ID = 0;
}

namespace llvm {
void initializeSouperPassPass(llvm::PassRegistry &);
}

INITIALIZE_PASS_BEGIN(SouperPass, "souper", "Souper super-optimizer pass",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DemandedBitsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LazyValueInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_END(SouperPass, "souper", "Souper super-optimizer pass", false,
                    false)

static struct Register {
  Register() {
    initializeSouperPassPass(*llvm::PassRegistry::getPassRegistry());
  }
} X;

static void registerSouperPass(
    const llvm::PassManagerBuilder &Builder, llvm::legacy::PassManagerBase &PM) {
  PM.add(new SouperPass);
}

static llvm::RegisterStandardPasses
#ifdef DYNAMIC_PROFILE_ALL
RegisterSouperOptimizer(llvm::PassManagerBuilder::EP_OptimizerLast,
                        registerSouperPass);
#else
RegisterSouperOptimizer(llvm::PassManagerBuilder::EP_Peephole,
                        registerSouperPass);
#endif
