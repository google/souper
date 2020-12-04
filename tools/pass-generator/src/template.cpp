#include "llvm/Pass.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/Transforms/InstCombine/InstCombineWorklist.h"

using namespace llvm;
using namespace llvm::PatternMatch;

namespace {

Value *getReplacement(llvm::Instruction *I, llvm::IRBuilder<> *B) {
//  Generate one block like the following for each
//  optimization derived by souper
  llvm::Value *a;
  if (match(I, m_Add(m_Value(a), m_Value(a)))) {
    return B->CreateShl(I->getOperand(0), 1);
  }

//#include "generated.cpp.inc"

  return nullptr;
}

struct SouperCombine : public FunctionPass {
  static char ID;
  SouperCombine() : FunctionPass(ID), Builder(TheContext) {
  }

  bool runOnFunction(Function &F) override {
    W.reserve(F.getInstructionCount());
    for (auto &BB : F) {
      for (auto &&I : BB) {
        W.push(&I);
      }
    }
    return run();
  }

  bool processInst(Instruction *I) {
    Builder.SetInsertPoint(I);
    if (auto V = getReplacement(I, &Builder)) {
      replace(I, V);
      return true;
    }
    return false;
  }
  void replace(Instruction *I, Value *V) {
    W.pushUsersToWorkList(*I);
    I->replaceAllUsesWith(V);
  }
  bool run() {
    bool Changed = false;
    while (auto I = W.removeOne()) {
      Changed = processInst(I) || Changed;
    }
    return Changed;
  }

  InstCombineWorklist W;

  LLVMContext TheContext;
  llvm::IRBuilder<> Builder;
};
}

char SouperCombine::ID = 0;
static RegisterPass<SouperCombine> X("souper-combine", "Souper Combine",
                             false /* Only looks at CFG */,
                             false /* Analysis Pass */);
