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
  typedef std::unordered_map<Inst *, llvm::APInt> InputVarInfo;

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

  class KnownBitsAnalysis {
    std::unordered_map<Inst*, llvm::KnownBits> KBCache;

    // Checks the cache or instruction metadata for knonwbits information
    bool cacheHasValue(Inst *I);

  public:
    KnownBitsAnalysis() {}
    KnownBitsAnalysis(std::unordered_map<Inst*, llvm::KnownBits> &Assumptions) {
      for (auto &P : Assumptions) {
        if (KBCache.find(P.first) != KBCache.end()) {
          llvm::KnownBits Existing = KBCache.find(P.first)->second;
          llvm::KnownBits Join;
          Join.Zero = P.second.Zero | Existing.Zero;
          Join.One = P.second.One | Existing.One;
          // What if this leads to a conflict?
          KBCache.insert({P.first, Join});
        } else {
          KBCache.insert({P.first, P.second});
        }
      }
    }

    llvm::KnownBits findKnownBits(Inst *I,
                                  ConcreteInterpreter &CI, bool UsePartialEval = true);

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
    ConstantRangeAnalysis() {}
    ConstantRangeAnalysis(std::unordered_map<Inst*, llvm::ConstantRange> &Assumptions) {
      for (auto &P : Assumptions) {
        if (CRCache.find(P.first) != CRCache.end()) {
          llvm::ConstantRange Existing = CRCache.find(P.first)->second;
          llvm::ConstantRange Join = Existing.intersectWith(P.second);
          CRCache.insert({P.first, Join});
          // TODO Attn: Is there a situation where this needs to be union?
        } else {
          CRCache.insert({P.first, P.second});
        }
      }
    }
    llvm::ConstantRange findConstantRange(souper::Inst *I,
                                          ConcreteInterpreter &CI, bool UsePartialEval = true);

    static llvm::ConstantRange findConstantRangeUsingSolver(souper::Inst *I,
                                                            Solver *S,
                                                            std::vector<InstMapping> &PCs);
  };

  class UseMap {
    void compute(Inst *I) {
      if (Map.find(I) != Map.end())
        return;

      std::set<Inst *> Accumulator;
      for (auto Op : I->Ops) {
        compute(Op);
        for (auto V : Map[Op])
          Accumulator.insert(V);
      }

      if (I->K == Inst::Var) {
        assert(Accumulator.empty());
        Accumulator.insert(I);
      }

      Map[I] = Accumulator;
    }

    std::unordered_map<Inst *, std::set<Inst *>> Map;

  public:
    std::set<Inst *> independentVars(Inst *A, Inst *B) {
      // Union - Intersection, vars used in A or B but not both
      compute(A);
      compute(B);
      std::set<Inst *> Result;
      for (auto V : Map[A]) {
        if (Map[B].find(V) == Map[B].end()) {
          Result.insert(V);
        }
      }
      for (auto V : Map[B]) {
        if (Map[A].find(V) == Map[A].end()) {
          Result.insert(V);
        }
      }
      return Result;
    }
  };

  struct RestrictedBitsAnalysis {
    std::unordered_map<Inst *, llvm::APInt> RBCache;
    llvm::APInt findRestrictedBits(souper::Inst *I);
  };

  class MustDemandedBitsAnalysis {
    UseMap Uses;
    RestrictedBitsAnalysis RB;
    std::unordered_map<Inst *, std::unordered_map<Inst *, llvm::APInt>> Cache;

    InputVarInfo findMustDemandedBitsImpl(souper::Inst *I);

  public:
    // One result per input var
    InputVarInfo findMustDemandedBits(souper::Inst *I);
  };

  // Simple version considering all used variables as "cared" for.
  // TODO Improve
  class DontCareBitsAnalysis {
  public:
    InputVarInfo findDontCareBits(souper::Inst *Root);
  };

  class HoleAnalysis {
  public:
    std::unordered_map<souper::Inst *, bool> Cache;
    bool findIfHole(souper::Inst *I);
  };

}

#endif
