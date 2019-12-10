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
#include "souper/Parser/Parser.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace llvm;
using namespace souper;

static cl::opt<std::string>
    InputFilename(cl::Positional,
                  cl::desc("<input souper RHS (default=stdin)>"),
                  cl::init("-"));

static cl::opt<std::string> OutputFilename(
    "o", cl::desc("<output destination for textual LLVM IR (default=stdout)>"),
    cl::init("-"));

static std::vector<llvm::Type *>
GetInputArgumentTypes(const InstContext &IC, llvm::LLVMContext &Context) {
  const std::vector<Inst *> AllVariables = IC.getVariables();

  std::vector<llvm::Type *> ArgTypes;
  ArgTypes.reserve(AllVariables.size());
  for (const Inst *const Var : AllVariables)
    ArgTypes.emplace_back(Type::getIntNTy(Context, Var->Width));

  return ArgTypes;
}

static std::map<Inst *, Value *> GetArgsMapping(const InstContext &IC,
                                                Function *F) {
  std::map<Inst *, Value *> Args;

  const std::vector<Inst *> AllVariables = IC.getVariables();
  for (auto zz : llvm::zip(AllVariables, F->args()))
    Args[std::get<0>(zz)] = &(std::get<1>(zz));

  return Args;
};

int Work(const MemoryBufferRef &MB) {
  InstContext IC;
  ReplacementContext RC;
  std::string ErrStr;

  const ParsedReplacement &RepRHS = ParseReplacementRHS(
      IC, MB.getBufferIdentifier(), MB.getBuffer(), RC, ErrStr);

  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  llvm::LLVMContext Context;
  llvm::Module Module("souper.ll", Context);

  const std::vector<llvm::Type *> ArgTypes = GetInputArgumentTypes(IC, Context);
  const auto FT = llvm::FunctionType::get(
      /*Result=*/Codegen::GetInstReturnType(Context, RepRHS.Mapping.RHS),
      /*Params=*/ArgTypes, /*isVarArg=*/false);

  Function *F = Function::Create(FT, Function::ExternalLinkage, "fun", &Module);

  const std::map<Inst *, Value *> Args = GetArgsMapping(IC, F);

  BasicBlock *BB = BasicBlock::Create(Context, "entry", F);

  llvm::IRBuilder<> Builder(Context);
  Builder.SetInsertPoint(BB);

  Value *RetVal = Codegen(Context, &Module, Builder, /*DT*/ nullptr,
                          /*ReplacedInst*/ nullptr, Args)
                      .getValue(RepRHS.Mapping.RHS);

  Builder.CreateRet(RetVal);

  // Validate the generated code, checking for consistency.
  if (verifyFunction(*F, &llvm::errs()))
    return 1;
  if (verifyModule(Module, &llvm::errs()))
    return 1;

  std::error_code EC;
  llvm::raw_fd_ostream OS(OutputFilename, EC);
  OS << Module;
  OS.flush();

  return 0;
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);

  auto MB = MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!MB) {
    llvm::errs() << MB.getError().message() << '\n';
    return 1;
  }

  return Work((*MB)->getMemBufferRef());
}
