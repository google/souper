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

#include "souper/Codegen/Codegen.h"
#include "souper/Inst/Inst.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <map>

#define DEBUG_TYPE "souper"

using namespace llvm;

namespace souper {

void optimizeModule(llvm::Module &M) {
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::FunctionPassManager FPM =
    PB.buildFunctionSimplificationPipeline(llvm::PassBuilder::OptimizationLevel::O2,
                                           ThinOrFullLTOPhase::None);
  llvm::ModulePassManager MPM;
  MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  MPM.run(M, MAM);
}

long getCodeSize(Module &M, TargetMachine *TM) {
  M.setDataLayout(TM->createDataLayout());
  SmallVector<char, 256> DotO;
  raw_svector_ostream dest(DotO);

  legacy::PassManager pass;
  if (TM->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) {
    errs() << "Target machine can't emit a file of this type";
    report_fatal_error("oops");
  }
  pass.run(M);

  SmallVectorMemoryBuffer Buf(std::move(DotO));
  auto ObjOrErr = object::ObjectFile::createObjectFile(Buf);
  if (!ObjOrErr)
    report_fatal_error("createObjectFile() failed");
  object::ObjectFile *OF = ObjOrErr.get().get();  
  auto SecList = OF->sections();
  long Size = 0;
  for (auto &S : SecList) {
    if (S.isText())
      Size += S.getSize();
  }
  if (Size > 0)
    return Size;
  else
    report_fatal_error("no text segment found");
}

struct TargetInfo {
  std::string Trip, CPU;
};

std::vector<TargetInfo> Targets {
  { "x86_64", "skylake" },
  { "aarch64", "apple-a12" },
};

bool Init = false;
  
void getBackendCost(InstContext &IC, souper::Inst *I, BackendCost &BC) {
  // TODO is this better than just forcing all clients of this code to
  // do the init themselves?
  if (!Init) {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();
    Init = true;
  }

  llvm::LLVMContext C;
  llvm::Module M("", C);
  if (genModule(IC, I, M))
    llvm::report_fatal_error("codegen error in getBackendCost()");

  optimizeModule(M);

  llvm::errs() << M;
  
  BackendCost Cost;
  for (auto &T : Targets) {
    std::string Error;
    auto Target = TargetRegistry::lookupTarget(T.Trip, Error);
    if (!Target) {
      errs() << Error;
      report_fatal_error("can't lookup target");
    }

    auto Features = "";
    TargetOptions Opt;
    auto RM = Optional<Reloc::Model>();
    auto TM = Target->createTargetMachine(T.Trip, T.CPU, Features, Opt, RM);

    Cost.C.push_back(getCodeSize(M, TM));
  }

  llvm::errs() << "cost vector: ";
  for (auto I : Cost.C) {
    llvm::errs() << I << " ";
  }
  llvm::errs() << "\n";
}

int threeWayCompare(int A, int B) {
  if (A < B)
    return -1;
  if (A > B)
    return 1;
  return 0;
}

// "The value returned indicates whether the element passed as first
// argument is considered to go before the second"
bool compareCosts(const BackendCost &C1, const BackendCost &C2) {
  assert(C1.C.size() == C2.C.size());

  int Count = 0;
  for (int i = 0; i < C1.C.size(); ++i)
    Count += threeWayCompare(C1.C[i], C2.C[i]);
  if (Count < 0)
    return true;
  if (Count > 0)
    return false;
  
  // break ties using souper cost?
  // break final ties how? we want a canonical winner for all cases
  // FIXME -- not finished
  return false;
}

} // namespace souper
