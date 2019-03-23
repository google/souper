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

EvalValue evaluateAddNSW(llvm::APInt a, llvm::APInt b);
EvalValue evaluateAddNUW(llvm::APInt a, llvm::APInt b);
EvalValue evaluateAddNW(llvm::APInt a, llvm::APInt b);
EvalValue evaluateSubNSW(llvm::APInt a, llvm::APInt b);
EvalValue evaluateSubNUW(llvm::APInt a, llvm::APInt b);
EvalValue evaluateSubNW(llvm::APInt a, llvm::APInt b);
EvalValue evaluateUDiv(llvm::APInt a, llvm::APInt b);
EvalValue evaluateURem(llvm::APInt a, llvm::APInt b);
EvalValue evaluateShl(llvm::APInt a, llvm::APInt b);
EvalValue evaluateLShr(llvm::APInt a, llvm::APInt b);
EvalValue evaluateAShr(llvm::APInt a, llvm::APInt b);

EvalValue evaluateInst(Inst* Root, ValueCache &Cache);
llvm::KnownBits findKnownBits(Inst* I, ValueCache& C, bool PartialEval = true);
llvm::KnownBits findKnownBitsUsingSolver(Inst *I, Solver *S, std::vector<InstMapping> &PCs);
llvm::ConstantRange findConstantRange(souper::Inst* I,
                                      souper::ValueCache& C,
                                      bool PartialEval = true);
llvm::ConstantRange findConstantRangeUsingSolver(souper::Inst* I, Solver *S, std::vector<InstMapping> &PCs);
bool isConcrete(souper::Inst *I, bool ConsiderConsts = true,
                                 bool ConsiderHoles = true);
std::string knownBitsString(llvm::KnownBits KB);
}


#endif
