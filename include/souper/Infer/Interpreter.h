#ifndef SOUPER_INTERPRTER_H
#define SOUPER_INTERPRTER_H

#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"

#include "souper/Inst/Inst.h"

namespace souper {

struct EvalValue {
  enum class ValueKind {Val, Poison, Undef, Unimplemented};

  EvalValue() : K(ValueKind::Unimplemented) {}
  EvalValue(llvm::APInt Val) : Value{Val}, K(ValueKind::Val) {};

  llvm::APInt Value;
  ValueKind K;

  bool hasValue() {
    return K == ValueKind::Val;
  }
  llvm::APInt getValue() {
    if (K == ValueKind::Val) {
      return Value;
    }
    llvm::report_fatal_error("souper::Interpreter: Value error");
  }

  static EvalValue poison() {
    EvalValue Result;
    Result.K = ValueKind::Poison;
    return Result;
  }
  static EvalValue undef() {
    EvalValue Result;
    Result.K = ValueKind::Undef;
    return Result;
  }
  static EvalValue unimplemented() {
    EvalValue Result;
    Result.K = ValueKind::Unimplemented;
    return Result;
  }

  template <typename Stream>
  void print(Stream &&Out) {
    switch (K) {
      case ValueKind::Val : Out << "Val"; break;
      case ValueKind::Poison : Out << "Poison"; break;
      case ValueKind::Undef : Out << "Undef"; break;
      case ValueKind::Unimplemented: Out << "Unimplemented"; break;
    }
    if (hasValue())
      Out << " : " << Value;
  }
};
using ValueCache = std::unordered_map<souper::Inst *, EvalValue>;

EvalValue evaluateInst(Inst* Root, ValueCache &Cache);
llvm::KnownBits findKnownBits(Inst* I, ValueCache& C);
llvm::ConstantRange findConstantRange(souper::Inst* I,
                                      souper::ValueCache& C);
}


#endif
