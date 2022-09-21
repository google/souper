#include "llvm/Pass.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
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
#include "llvm/Support/Debug.h"
#define DEBUG_TYPE ""
#include "llvm/Transforms/Utils/InstructionWorklist.h"
#include "llvm/Support/KnownBits.h"

#include <map>

using namespace llvm;
using namespace llvm::PatternMatch;

// TODO Match trees
// TODO Make the commutative operations work
// This is critical because commutative operations
// sometimes have the arguments inverted for canonicalization
 
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
  
  bool match_nth(size_t n, Value *V) {
    switch (n) {
      case 0 : return std::get<0>(Matchers).match(V);
      case 1 : return std::get<1>(Matchers).match(V);
      // case 2 : return std::get<2>(Matchers).match(V);
      // case 3 : return std::get<3>(Matchers).match(V);
      // case 4 : return std::get<4>(Matchers).match(V);
      default: return false;
      // TODO Add more if needed.
    }
  }

  bool check(const Value *V) {
    if (auto Phi = dyn_cast<PHINode>(V)) {
      for (size_t i =0; i < Phi->getNumOperands(); ++i) {
        // bool Matched = false;
        // std::apply([&](auto &&... args){
        //   ((Matched |= args.match(Phi->getOperand(i))), ...);
        // }, Matchers);

        // if (!Matched) {
        //   return false;
        // }
        if (!match_nth(i, Phi->getOperand(i))) {
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

struct bind_apint {
  APInt &VR;

  bind_apint(APInt &V) : VR(V) {}

  template <typename ITy> bool match(ITy *V) {
    if (const auto *CV = dyn_cast<ConstantInt>(V)) {
      VR = CV->getValue();
      return true;
    } else {
      return false;
    }
    return false;
  }
};

struct width_specific_intval : public specific_intval<false> {
  size_t Width;
  width_specific_intval(llvm::APInt V, size_t W) : specific_intval<false>(V), Width(W) {}
  
  template <typename ITy> bool match(ITy *V) {
    if (V->getType()->getScalarSizeInBits() != Width) {
      return false;
    }
    return specific_intval<false>::match(V);
  }
};

// FIXME Widths beyond 64
inline width_specific_intval m_SpecificInt(size_t W, uint64_t V) {
  return width_specific_intval(APInt(64, V), W);
}

struct constant_matcher {
  llvm::Value** Captured;
  constant_matcher(llvm::Value** C) : Captured(C) {}
  template <typename OpTy> bool match(OpTy *V) {
    if (isa<ConstantInt>(V)) {
      *Captured = dyn_cast<ConstantInt>(V);
      return *Captured != nullptr;
    }
    return false;
  }
};

inline constant_matcher m_Constant(Value **V) {
  return constant_matcher(V);
}

// Tested, matches APInts
inline bind_apint m_APInt(APInt &V) { return bind_apint(V); }

// TODO: Match (arbitrarily) constrained APInts


template <typename Op_t, unsigned Opcode> struct CastClass_match_width {
  size_t Width;
  Op_t Op;

  CastClass_match_width(size_t W, const Op_t &OpMatch) : Width(W), Op(OpMatch) {}

  template <typename OpTy> bool match(OpTy *V) {
    if (V->getType()->getScalarSizeInBits() != Width) {
      return false;
    }
    if (auto *O = dyn_cast<Operator>(V))
      return O->getOpcode() == Opcode && Op.match(O->getOperand(0));
    return false;
  }
};

template<typename Matcher>
struct Capture {
  Value **Captured;
  Matcher M;

  template<typename ...CArgs>
  explicit Capture(Value **V, CArgs ...C) : Captured(V), M(C...) {}

  template <typename OpTy> bool match(OpTy *V) {
    if (M.match(V)) {
      *Captured = dyn_cast<Value>(V);
      if (!*Captured) {
        llvm::errs() << "Capture failed.\n";
        return false;
      }
      return true;
    } else {
      *Captured = nullptr;
      return false;
    }
  }
};

template<typename Matcher>
Capture<Matcher> Cap(Value **V, Matcher &&M) {
  return Capture<Matcher>(V, M);
}

// Equivalent to the Cap function
template <typename Matcher>
Capture<Matcher> operator<<=(Value **V, Matcher &&M) {
  return Capture<Matcher>(V, M);
}

template <typename OpTy>
inline CastClass_match_width<OpTy, Instruction::ZExt> m_ZExt(size_t W, const OpTy &Op) {
  return CastClass_match_width<OpTy, Instruction::ZExt>(W, Op);
}

template <typename OpTy>
inline CastClass_match_width<OpTy, Instruction::SExt> m_SExt(size_t W, const OpTy &Op) {
  return CastClass_match_width<OpTy, Instruction::SExt>(W, Op);
}

template <typename OpTy>
inline CastClass_match_width<OpTy, Instruction::Trunc> m_Trunc(size_t W, const OpTy &Op) {
  return CastClass_match_width<OpTy, Instruction::Trunc>(W, Op);
}

// Utility functions to use in the DSL
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

  bool dc(llvm::DominatorTree *DT, llvm::Instruction *I, llvm::Value *V) {
    if (auto Def = dyn_cast<Instruction>(V)) {
      if (I->getParent() == Def->getParent()) {
        return true;
      }
      return DT->dominates(Def, I->getParent());
    }
    return true;
  }

  bool check_width(llvm::Value *V, size_t W) {
   return V->getType()->getScalarSizeInBits() == W;
  }

  template<typename Out, typename FT, typename ...Args>
  bool check_related(Out Result, FT F, Args... args) {
    return Result == F(args...);
  }

  bool pow2(llvm::Value *V) {
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

  bool nz(llvm::Value *V) {
    if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
      return !Con->getValue().isZero();
    }
//    llvm::errs() << "NZ called on NC.\n";
    return false;
  }

  bool nn(llvm::Value *V) {
    if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
      return Con->getValue().isNonNegative();
    }
//    llvm::errs() << "NN called on NC.\n";
    return false;
  }

  bool neg(llvm::Value *V) {
    if (ConstantInt *Con = llvm::dyn_cast<ConstantInt>(V)) {
      return Con->getValue().isNegative();
    }
//    llvm::errs() << "Neg called on NC.\n";
    return false;
  }

  struct Stats {
    void hit(size_t opt) {
      Hits[opt]++;
    }
//    void dcmiss(size_t opt) {
//      DCMiss[opt]++;
//    }
    std::map<size_t, size_t> Hits;
//    std::map<size_t, size_t> DCMiss;
    void print() {
      std::vector<std::pair<size_t, size_t>> Copy(Hits.size(), std::make_pair(0, 0));
      std::copy(Hits.begin(), Hits.end(), Copy.begin());
      std::sort(Copy.begin(), Copy.end(),
                [](auto &A, auto &B) {return A.second > B.second;});
      llvm::errs() << "Hits begin:\n";
      size_t sum = 0;
      for (auto &&P : Copy) {
        sum += P.second;
        llvm::errs() << "OptID " << P.first << " matched " << P.second << " time(s).\n";
      }
      llvm::errs() << "Hits end. Total = " << sum << ".\n";
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
  ~SouperCombine() {
    St.print();
  }

  bool runOnFunction(Function &F) override {
    DT = new DominatorTree(F);
    
    W.reserve(F.getInstructionCount());
    for (auto &BB : F) {
      for (auto &&I : BB) {
        W.push(&I);
      }
    }
    IRBuilder Builder(F.getContext());
    // llvm::errs() << "Before:\n" << F;
    auto r = run(Builder);
    // llvm::errs() << "After:\n" << F;
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
    return Changed;
  }
  
  Value *getReplacement(llvm::Instruction *I, IRBuilder *B) {
    if (!I->hasOneUse()) {
      return nullptr;
    }

    // Interestingly commenting out ^this block
    // slightly improves results.
    // Implying this situation can be improved further

    // Autogenerated transforms
    #include "gen.cpp.inc"
    
    return nullptr;
  }
  
  Value *C(size_t Width, size_t Value, IRBuilder *B) {
    return B->getIntN(Width, Value);
  }

  Value *C(llvm::APInt Value, IRBuilder *B) {
    return B->getIntN(Value.getBitWidth(), Value.getLimitedValue());
    // FIXME: Figure out how to make a value from a full APInt.
  }

  Type *T(size_t W, IRBuilder *B) {
    return B->getIntNTy(W);
  }

  InstructionWorklist W;
  util::Stats St;
  DominatorTree *DT;
};
}

char SouperCombine::ID = 0;
namespace llvm {
void initializeSouperCombinePass(llvm::PassRegistry &);
}

INITIALIZE_PASS_BEGIN(SouperCombine, "souper-combine", "Souper super-optimizer pass",
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
