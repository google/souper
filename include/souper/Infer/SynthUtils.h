#ifndef SOUPER_SYNTH_UTILS_H
#define SOUPER_SYNTH_UTILS_H

#include "souper/Inst/Inst.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Parser/Parser.h"
#include "souper/Infer/Pruning.h"

namespace souper {

// TODO: Lazy construction instead of eager.
// eg: Instead of Builder(I, IC).Add(1)()
// we could do Builder(I).Add(1)(IC)
class Builder {
public:
  Builder(Inst *I_, InstContext &IC_) : I(I_), IC(IC_) {}
  Builder(InstContext &IC_, Inst *I_) : I(I_), IC(IC_) {}
  Builder(InstContext &IC_, llvm::APInt Value) : IC(IC_) {
    I = IC.getConst(Value);
  }
  Builder(Inst *I_, InstContext &IC_, uint64_t Value) : IC(IC_) {
    I = IC.getConst(llvm::APInt(I_->Width, Value));
  }

  Inst *operator()() {
    assert(I);
    return I;
  }

#define BINOP(K)                                                 \
  template<typename T> Builder K(T t) {                          \
    auto L = I; auto R = i(t, *this);                            \
    return Builder(IC.getInst(Inst::K, L->Width, {L, R}), IC);   \
  }

  BINOP(Add) BINOP(Sub) BINOP(Mul)
  BINOP(And) BINOP(Xor) BINOP(Or)
  BINOP(Shl) BINOP(LShr) BINOP(UDiv)
  BINOP(SDiv)
#undef BINOP

  template<typename T> Builder Ugt(T t) {                        \
    auto L = I; auto R = i(t, *this);                            \
    return Builder(IC.getInst(Inst::Ult, 1, {R, L}), IC); \
  }

#define BINOPW(K)                                                \
  template<typename T> Builder K(T t) {                          \
    auto L = I; auto R = i(t, *this);                            \
    return Builder(IC.getInst(Inst::K, 1, {L, R}), IC);          \
  }
  BINOPW(Slt) BINOPW(Ult) BINOPW(Sle) BINOPW(Ule)
  BINOPW(Eq) BINOPW(Ne)
#undef BINOPW

#define UNOP(K)                                                  \
  Builder K() {                                                  \
    auto L = I;                                                  \
    return Builder(IC.getInst(Inst::K, L->Width, {L}), IC);      \
  }
  UNOP(LogB) UNOP(BitReverse) UNOP(BSwap) UNOP(Cttz) UNOP(Ctlz)
  UNOP(BitWidth) UNOP(CtPop)
#undef UNOP

  Builder Flip() {
    auto L = I;
    auto AllOnes = IC.getConst(llvm::APInt::getAllOnesValue(L->Width));
    return Builder(IC.getInst(Inst::Xor, L->Width, {L, AllOnes}), IC);
  }
  Builder Negate() {
    auto L = I;
    auto Zero = IC.getConst(llvm::APInt(L->Width, 0));
    return Builder(IC.getInst(Inst::Sub, L->Width, {Zero, L}), IC);
  }

#define UNOPW(K)                                                 \
  Builder K(size_t W) {                                          \
    auto L = I;                                                  \
    return Builder(IC.getInst(Inst::K, W, {L}), IC);             \
  }
  UNOPW(ZExt) UNOPW(SExt) UNOPW(Trunc)
#undef UNOPW

private:
  Inst *I = nullptr;
  InstContext &IC;

  Inst *i(Builder A, Inst *I) {
    assert(A.I);
    return A.I;
  }

  template<typename N>
  Inst *i(N Number, Builder B) {
    return B.IC.getConst(llvm::APInt(B.I->Width, Number, false));
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

Inst *Replace(Inst *R, InstContext &IC, std::map<Inst *, Inst *> &M);
ParsedReplacement Replace(ParsedReplacement I, InstContext &IC,
                          std::map<Inst *, Inst *> &M);

Inst *Clone(Inst *R, InstContext &IC);

InstMapping Clone(InstMapping In, InstContext &IC);

ParsedReplacement Clone(ParsedReplacement In, InstContext &IC);

// Also Synthesizes given constants
// Returns clone if verified, nullptrs if not
std::optional<ParsedReplacement> Verify(ParsedReplacement Input, InstContext &IC, Solver *S);
// bool IsValid(ParsedReplacement Input, InstContext &IC, Solver *S);

std::map<Inst *, llvm::APInt> findOneConstSet(ParsedReplacement Input, const std::set<Inst *> &SymCS, InstContext &IC, Solver *S);

std::vector<std::map<Inst *, llvm::APInt>> findValidConsts(ParsedReplacement Input, const std::set<Inst *> &Insts, InstContext &IC, Solver *S, size_t MaxCount);

ValueCache GetCEX(const ParsedReplacement &Input, InstContext &IC, Solver *S);

std::vector<ValueCache> GetMultipleCEX(ParsedReplacement Input, InstContext &IC, Solver *S, size_t MaxCount);

int profit(const ParsedReplacement &P);

}
#endif
