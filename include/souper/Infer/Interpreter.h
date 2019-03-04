#ifndef SOUPER_INTERPRTER_H
#define SOUPER_INTERPRTER_H

#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"

#include "souper/Inst/Inst.h"

#include <unordered_map>

namespace souper {

struct EvalValue {
  enum class ValueKind {Val, Poison, Undef, UB, Unimplemented};

  EvalValue() : K(ValueKind::Unimplemented) {}
  EvalValue(llvm::APInt Val) : Value{Val}, K(ValueKind::Val) {};

  llvm::APInt Value;
  ValueKind K;

  bool hasValue() {
    return K == ValueKind::Val;
  }
  llvm::APInt getValue() {
    if (K != ValueKind::Val) {
      llvm::errs() << "Interpreter: expected number but got ";
      print(llvm::errs());
      llvm::errs() << "\n";
      llvm::report_fatal_error("exiting");
    }
    return Value;
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
  static EvalValue ub() {
    EvalValue Result;
    Result.K = ValueKind::UB;
    return Result;
  }

  template <typename Stream>
  void print(Stream &&Out) {
    Out << "Value: ";
    switch (K) {
      case ValueKind::Val : Out << Value; break;
      case ValueKind::Poison : Out << "Poison"; break;
      case ValueKind::Undef : Out << "Undef"; break;
      case ValueKind::Unimplemented : Out << "Unimplemented"; break;
      case ValueKind::UB : Out << "Undefined Behavior"; break;
    }
  }
};
using ValueCache = std::unordered_map<souper::Inst *, EvalValue>;

EvalValue evaluateInst(Inst* Root, ValueCache &Cache);
llvm::KnownBits findKnownBits(Inst* I, ValueCache& C, bool PartialEval = true);
llvm::ConstantRange findConstantRange(souper::Inst* I,
                                      souper::ValueCache& C,
                                      bool PartialEval = true);
bool isConcrete(souper::Inst *I, bool ConsiderConsts = true,
                                 bool ConsiderHoles = true);
}


#endif
