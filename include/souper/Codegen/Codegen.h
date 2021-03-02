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

#ifndef SOUPER_CODEGEN_CODEGEN_H
#define SOUPER_CODEGEN_CODEGEN_H

#include "souper/Inst/Inst.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include <map>

namespace souper {

class Codegen {
  llvm::LLVMContext &Context;
  llvm::Module *M;
  llvm::Function *F;
  llvm::IRBuilder<> &Builder;
  llvm::DominatorTree *DT;

  llvm::Instruction *ReplacedInst;
  const std::map<Inst *, llvm::Value *> &ReplacedValues;
  
  llvm::Value *getValueHelper(Inst *I, std::map<Inst *, llvm::Value *> &Cache, std::map<Block *, llvm::Value *> &BM);
  bool GenControlFlow = false;
public:
  Codegen(llvm::LLVMContext &Context_, llvm::Module *M_,
          llvm::IRBuilder<> &Builder_, llvm::DominatorTree *DT_,
          llvm::Instruction *ReplacedInst_,
          const std::map<Inst *, llvm::Value *> &ReplacedValues_,
          bool GenControlFlow_)
      : Context(Context_), M(M_), Builder(Builder_), DT(DT_),
        ReplacedInst(ReplacedInst_), ReplacedValues(ReplacedValues_),
        GenControlFlow(GenControlFlow_) {}

  static llvm::Type *GetInstReturnType(llvm::LLVMContext &Context, Inst *I);

  llvm::Function *getFunction(Inst *I, const InstContext &IC);
  llvm::Value *getValue(Inst *I);
};

} // namespace souper

#endif // SOUPER_CODEGEN_CODEGEN_H
