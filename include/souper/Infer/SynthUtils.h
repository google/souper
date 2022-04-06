#ifndef SOUPER_SYNTH_UTILS_H
#define SOUPER_SYNTH_UTILS_H

#include "souper/Inst/Inst.h"
#include "souper/Infer/EnumerativeSynthesis.h"

namespace souper {

class Builder {
public:
  Builder(Inst *I_, InstContext &IC_) : I(I_), IC(IC_) {}

  Inst *operator()() {
    assert(I);
    return I;
  }

#define BINOP(K)                                                 \
  template<typename T> Builder K(T t) {                          \
    auto L = I; auto R = i(t, *this);                            \
    return Builder(IC.getInst(Inst::K, L->Width, {L, R}), IC);   \
  }

  BINOP(Add) BINOP(Sub) BINOP(And)
#undef BINOP

#define BINOPW(K)                                                \
  template<typename T> Builder K(T t) {                          \
    auto L = I; auto R = i(t, *this);                            \
    return Builder(IC.getInst(Inst::K, 1, {L, R}), IC);          \
  }
  BINOPW(Slt) BINOPW(Ult) BINOPW(Eq)
#undef BINOPW

private:
  Inst *I = nullptr;
  InstContext &IC;

  Inst *i(Builder A, Inst *I) {
    assert(A.I);
    return A.I;
  }

  template<typename N>
  Inst *i(N Number, Builder B) {
    return B.IC.getConst(llvm::APInt(B.I->Width, Number));
  }

  template<>
  Inst *i<Inst *>(Inst *I, Builder B) {
    assert(I);
    return I;
  }

  template<>
  Inst *i<Builder>(Builder A, Builder B) {
    assert(A.I);
    return A.I;
  }

  template<>
  Inst *i<std::string>(std::string Number, Builder B) {
    return B.IC.getConst(llvm::APInt(B.I->Width, Number, 10));
  }

  template<>
  Inst *i<llvm::APInt>(llvm::APInt Number, Builder B) {
    return B.IC.getConst(Number);
  }
};

Inst *Replace(Inst *R, InstContext &IC, std::map<Inst *, Inst *> &M) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  return getInstCopy(R, IC, M, BlockCache, &ConstMap, false);
}

Inst *Clone(Inst *R, InstContext &IC) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  std::map<Inst *, Inst *> InstCache;
  return getInstCopy(R, IC, InstCache, BlockCache, &ConstMap, true, false);
}

InstMapping Clone(InstMapping In, InstContext &IC) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  std::map<Inst *, Inst *> InstCache;
  InstMapping Out;
  Out.LHS = getInstCopy(In.LHS, IC, InstCache, BlockCache, &ConstMap, true, false);
  Out.RHS = getInstCopy(In.RHS, IC, InstCache, BlockCache, &ConstMap, true, false);
  return Out;
}



}
#endif
