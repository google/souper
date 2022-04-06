#ifndef SOUPER_SYNTH_UTILS_H
#define SOUPER_SYNTH_UTILS_H

#include "souper/Inst/Inst.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/GetSolver.h"

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

// Also Synthesizes given constants
// Returns clone if verified, nullptrs if not
InstMapping Verify(ParsedReplacement Input, InstContext &IC, Solver *S) {
  std::set<Inst *> ConstSet;
  souper::getConstants(Input.Mapping.RHS, ConstSet);
  if (!ConstSet.empty()) {
    std::map <Inst *, llvm::APInt> ResultConstMap;
    ConstantSynthesis CS;
    auto SMTSolver = GetUnderlyingSolver();
    auto EC = CS.synthesize(SMTSolver.get(), Input.BPCs, Input.PCs,
                         Input.Mapping, ConstSet,
                         ResultConstMap, IC, /*MaxTries=*/30, 10,
                         /*AvoidNops=*/true);
    if (!ResultConstMap.empty()) {
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      auto LHSCopy = getInstCopy(Input.Mapping.LHS, IC, InstCache, BlockCache, &ResultConstMap, true);
      auto RHS = getInstCopy(Input.Mapping.RHS, IC, InstCache, BlockCache, &ResultConstMap, true);
      return InstMapping(LHSCopy, RHS);
//      Results.push_back(CandidateReplacement(/*Origin=*/nullptr, InstMapping(LHSCopy, RHS)));
    } else {
      if (DebugLevel > 2) {
        llvm::errs() << "Constant Synthesis ((no Dataflow Preconditions)) failed. \n";
      }
    }
    return InstMapping(nullptr, nullptr);
  }
  std::vector<std::pair<Inst *, llvm::APInt>> Models;
  bool IsValid;
  if (auto EC = S->isValid(IC, Input.BPCs, Input.PCs, Input.Mapping, IsValid, &Models)) {
    llvm::errs() << EC.message() << '\n';
  }
  if (IsValid) {
//    Results.push_back(CandidateReplacement(/*Origin=*/nullptr, Clone(Mapping, IC)));
    return Clone(Input.Mapping, IC);
  } else {
    return InstMapping(nullptr, nullptr);
  }
}

}
#endif
