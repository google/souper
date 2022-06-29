#include "llvm/Pass.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/InitializePasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombineWorklist.h"
#include "llvm/Support/KnownBits.h"

using namespace llvm;
using namespace llvm::PatternMatch;

namespace {

// Custom Creators

class IRBuilder : public llvm::IRBuilder<NoFolder> {
public:
  IRBuilder(llvm::LLVMContext &C) : llvm::IRBuilder<NoFolder>(C) {}

  llvm::Value *CreateLogB(llvm::Value *V) {
    if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
      auto Result = Con->getValue().logBase2();
      return ConstantInt::get(Con->getType(), Result);
    } else {
      llvm_unreachable("Panic, has to be guarded in advance!");
    }
  }

  // TODO Verify that these work, the mangling argument is weird
  llvm::Value *CreateFShl(llvm::Value *A, llvm::Value *B, llvm::Value *C) {
    return CreateIntrinsic(Intrinsic::fshl, {A->getType()}, {A, B, C});
  }
  llvm::Value *CreateFShr(llvm::Value *A, llvm::Value *B, llvm::Value *C) {
    return CreateIntrinsic(Intrinsic::fshr, {A->getType()}, {A, B, C});
  }
  llvm::Value *CreateBSwap(llvm::Value *A) {
    return CreateIntrinsic(Intrinsic::bswap, {A->getType()}, {A});
  }
};


// Custom Matchers

static constexpr auto NWFlag = OverflowingBinaryOperator::NoSignedWrap
    | OverflowingBinaryOperator::NoUnsignedWrap;
#define NWT(OP) OverflowingBinaryOp_match<LHS, RHS, Instruction::OP, NWFlag>
#define NWM(OP) \
template <typename LHS, typename RHS> NWT(OP) \
m_NW##OP(const LHS &L, const RHS &R) { \
  return NWT(OP)(L, R); \
}

NWM(Add)
NWM(Sub)
NWM(Mul)
NWM(Shl)

#undef NWM
#undef NWT

template <typename ...Args>
struct phi_match {
  phi_match(Args... args) : Matchers{args...} {};
  std::tuple<Args...> Matchers;

  bool check(const Value *V) {
    if (auto Phi = dyn_cast<PHINode>(V)) {
      // Every Phi Argument has to match with at least one matcher
      for (size_t i =0; i < Phi->getNumOperands(); ++i) {
        bool Matched = false;
        std::apply([&](auto &&... args){
          ((Matched |= args.match(Phi->getOperand(i))), ...);
        }, Matchers);

        if (!Matched) {
          return false;
        }
      }
      return true;
    }
    return false;
  }

  template <typename I> bool match(I *V) {
    return check(V);
  }
};

// TODO Test semantics, it compiles and runs.
template<typename ...Args>
phi_match<Args...> m_Phi(Args... args) {
  return phi_match<Args...>(args...);
}


namespace util {
  Value *node(Instruction *I, const std::vector<size_t> &Path) {
    Value *Current = I;
    for (auto &&P : Path) {
      if (Instruction *CI = dyn_cast<Instruction>(Current)) {
        if (CI->getNumOperands() > P) {
          Current = CI->getOperand(P);
        } else {
          return nullptr;
        }
      } else {
        return nullptr;
      }
    }
    return Current;
  }

  bool check_width(llvm::Value *V, size_t W) {
   return V->getType()->getScalarSizeInBits() == W;
  }

  bool cpow2(llvm::Value *V) {
    if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
      if (Con->getValue().isPowerOf2()) {
        return true;
      }
    }
    return false;
  }

  bool IsKBSubset(KnownBits Small, KnownBits Big) {
    // FIXME Think about this carefully.
    // It can't just be conflict.

    return false;
  }

  bool IsCRSubset(ConstantRange Small, ConstantRange Big) {
    return Big.contains(Small);
  }

    bool ckb(llvm::Value *V, llvm::KnownBits Overapprox) {
      if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
        auto Val = Con->getUniqueInteger();
        llvm::KnownBits KB(V->getType()->getIntegerBitWidth());
        KB.One = Val;
        KB.Zero = ~Val;
        return IsKBSubset(KB, Overapprox);
      }
      return false;
    }

    bool ccr(llvm::Value *V,llvm::ConstantRange R) {
      if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
        return R.contains(Con->getUniqueInteger());
      }
      return false;
    }

  bool vkb(llvm::Value *V, llvm::KnownBits OverApprox) {
    auto Analyzed = llvm::KnownBits(V->getType()->getIntegerBitWidth());
    if (Instruction *I = llvm::dyn_cast<Instruction>(V)) {
      DataLayout DL(I->getParent()->getParent()->getParent());
      computeKnownBits(V, Analyzed, DL, 4);
    }
    return IsKBSubset(Analyzed, OverApprox);
  }

  bool vcr(llvm::Value *V, llvm::ConstantRange R) {
    // FIXME obtain result from range analysis pass
    return false;
  }


  struct Stats {
    void hit(size_t opt) {
      Hits[opt]++;
    }
    std::map<size_t, size_t> Hits;
    void print() {
      std::vector<std::pair<size_t, size_t>> Copy(Hits.size(), std::make_pair(0, 0));
      std::copy(Hits.begin(), Hits.end(), Copy.begin());
      std::sort(Copy.begin(), Copy.end(),
                [](auto &A, auto &B) {return A.second > B.second;});
      llvm::errs() << "Hits begin:\n";
      for (auto &&P : Copy) {
        llvm::errs() << "OptID " << P.first << " matched " << P.second << " time(s).\n";
      }
      llvm::errs() << "Hits end.\n";
    }
  };
  bool nc(llvm::Value *a, llvm::Value *b) {
    if (llvm::isa<llvm::Constant>(a) || llvm::isa<llvm::Constant>(b)) return false;
    return true;
  }
}
struct SouperCombine : public FunctionPass {
  static char ID;
  SouperCombine() : FunctionPass(ID) {
  }

  bool runOnFunction(Function &F) override {
    W.reserve(F.getInstructionCount());
    for (auto &BB : F) {
      for (auto &&I : BB) {
        W.push(&I);
      }
    }
    IRBuilder Builder(F.getContext());
    llvm::errs() << "Before:\n" << F;
    auto r = run(Builder);
    llvm::errs() << "After:\n" << F;
    return r;
  }

  bool processInst(Instruction *I, IRBuilder &Builder) {
    Builder.SetInsertPoint(I);
    if (auto V = getReplacement(I, &Builder)) {
      replace(I, V, Builder);
      return true;
    }
    return false;
  }
  void replace(Instruction *I, Value *V, IRBuilder &Builder) {
    W.pushUsersToWorkList(*I);
    I->replaceAllUsesWith(V);
  }
  bool run(IRBuilder &Builder) {
    bool Changed = false;
    while (auto I = W.removeOne()) {
      Changed = processInst(I, Builder) || Changed;
    }

    St.print();

    return Changed;
  }
  
  Value *getReplacement(llvm::Instruction *I, IRBuilder *B) {
    // Autogenerated transforms
#include "gen.cpp.inc"
    return nullptr;
  }
  
  Value *C(size_t Width, size_t Value, IRBuilder *B) {
    return B->getIntN(Width, Value);
  }

  Type *T(size_t W, IRBuilder *B) {
    return B->getIntNTy(W);
  }

  InstCombineWorklist W;
  util::Stats St;
};
}

char SouperCombine::ID = 0;
namespace llvm {
void initializeSouperCombinePass(llvm::PassRegistry &);
}

INITIALIZE_PASS_BEGIN(SouperCombine, "souper", "Souper super-optimizer pass",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DemandedBitsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LazyValueInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_END(SouperCombine, "souper-combine", "Souper super-optimizer pass", false,
                    false)

static struct Register {
  Register() {
    initializeSouperCombinePass(*llvm::PassRegistry::getPassRegistry());
  }
} X;

static void registerSouperCombine(
    const llvm::PassManagerBuilder &Builder, llvm::legacy::PassManagerBase &PM) {
  PM.add(new SouperCombine);
}

static llvm::RegisterStandardPasses
RegisterSouperOptimizer(llvm::PassManagerBuilder::EP_Peephole,
                        registerSouperCombine);
