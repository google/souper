// Copyright 2019 The Souper Authors. All rights reserved.
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

#ifndef SOUPER_INTERPRTER_H
#define SOUPER_INTERPRTER_H

#include "souper/Extractor/Solver.h"
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

EvalValue evaluateAddNSW(llvm::APInt A, llvm::APInt B);
EvalValue evaluateAddNUW(llvm::APInt A, llvm::APInt B);
EvalValue evaluateAddNW(llvm::APInt A, llvm::APInt B);
EvalValue evaluateSubNSW(llvm::APInt A, llvm::APInt B);
EvalValue evaluateSubNUW(llvm::APInt A, llvm::APInt B);
EvalValue evaluateSubNW(llvm::APInt A, llvm::APInt B);
EvalValue evaluateUDiv(llvm::APInt A, llvm::APInt B);
EvalValue evaluateURem(llvm::APInt A, llvm::APInt B);
EvalValue evaluateShl(llvm::APInt A, llvm::APInt B);
EvalValue evaluateLShr(llvm::APInt A, llvm::APInt B);
EvalValue evaluateAShr(llvm::APInt A, llvm::APInt B);

  class ConcreteInterpreter {
    ValueCache Cache;
    bool CacheWritable = false;

    EvalValue evaluateSingleInst(Inst *I, std::vector<EvalValue> &Args);

  public:
    ConcreteInterpreter() {}
    ConcreteInterpreter(ValueCache &Input) : Cache(Input) {}
    ConcreteInterpreter(Inst *I, ValueCache &Input) : Cache(Input) {
      CacheWritable = true;
      evaluateInst(I);
      CacheWritable = false;
    }

    EvalValue evaluateInst(Inst *Root);
  };

}


#endif
