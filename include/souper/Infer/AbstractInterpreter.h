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

#ifndef SOUPER_ABSTRACT_INTERPRTER_H
#define SOUPER_ABSTRACT_INTERPRTER_H

#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"

#include "souper/Inst/Inst.h"
#include "souper/Infer/Interpreter.h"

#include <unordered_map>

namespace souper {
  namespace BinaryTransferFunctionsKB {
    llvm::KnownBits add(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits addnsw(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits sub(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits subnsw(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits mul(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits udiv(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits urem(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits and_(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits or_(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits xor_(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits shl(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits lshr(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits ashr(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits eq(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits ne(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits ult(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits slt(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits ule(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
    llvm::KnownBits sle(const llvm::KnownBits &LHS, const llvm::KnownBits &RHS);
  }

  namespace BinaryTransferFunctionsCR {
    llvm::ConstantRange binaryOr(const llvm::ConstantRange &LHS, const llvm::ConstantRange &RHS);
    llvm::ConstantRange binaryAnd(const llvm::ConstantRange &LHS, const llvm::ConstantRange &RHS);
  }

  bool isConcrete(souper::Inst *I,
                  bool ConsiderConsts = true,
                  bool ConsiderHoles = true);

  void improveKBCR(llvm::KnownBits &KB, llvm::ConstantRange &CR);

  class KnownBitsAnalysis {
    std::unordered_map<Inst*, llvm::KnownBits> KBCache;

    // Checks the cache or instruction metadata for knonwbits information
    bool cacheHasValue(Inst *I);

  public:
    llvm::KnownBits findKnownBits(Inst *I,
                                  ConcreteInterpreter &CI);

    static llvm::KnownBits findKnownBitsUsingSolver(Inst *I,
                                                    Solver *S,
                                                    std::vector<InstMapping> &PCs);

    static std::string knownBitsString(llvm::KnownBits KB);

    static llvm::KnownBits getMostPreciseKnownBits(llvm::KnownBits A, llvm::KnownBits B);

    static llvm::KnownBits mergeKnownBits(std::vector<llvm::KnownBits> Vec);

    static bool isConflictingKB(const llvm::KnownBits &A, const llvm::KnownBits &B);
  };

  class ConstantRangeAnalysis {
    std::unordered_map<Inst*, llvm::ConstantRange> CRCache;

    // checks the cache or instruction metadata for cr information
    bool cacheHasValue(Inst *I);

  public:
    llvm::ConstantRange findConstantRange(souper::Inst *I,
                                          ConcreteInterpreter &CI);

    static llvm::ConstantRange findConstantRangeUsingSolver(souper::Inst *I,
                                                            Solver *S,
                                                            std::vector<InstMapping> &PCs);
  };
}

#endif
