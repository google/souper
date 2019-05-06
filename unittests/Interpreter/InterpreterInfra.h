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

#ifndef SOUPER_INTERPRTER_INFRA_H
#define SOUPER_INTERPRTER_INFRA_H

#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"

#include "souper/Infer/Interpreter.h"
#include "souper/Infer/AbstractInterpreter.h"

#include <bitset>

// width used for transfer function tests
constexpr int WIDTH = 4;

namespace souper {

    struct EvalValueKB : public EvalValue {
      llvm::KnownBits ValueKB;

      EvalValueKB(llvm::KnownBits Val);
      EvalValueKB(EvalValue &Val);
      llvm::KnownBits getValueKB();
    };

    class KBTesting {
      // wrapper over @llvm::KnownBitsAnalysis::mergeKnownBits()
      EvalValueKB merge(EvalValueKB a, EvalValueKB b);

      llvm::KnownBits setLowest(llvm::KnownBits x);
      llvm::KnownBits clearLowest(llvm::KnownBits x);

      EvalValueKB bruteForce(llvm::KnownBits x, llvm::KnownBits y, Inst::Kind Pred);
    public:
      static bool nextKB(llvm::KnownBits &x);
      bool testFn(Inst::Kind pred);
    };



    class CRTesting {
      // Checks if the range @R contains all values given in the @Table
      bool rangeContainsAll(const llvm::ConstantRange &R, const bool Table[]);

      // Find the largest hole and build a llvm::ConstantRange around it
      llvm::ConstantRange bestCR(const bool Table[], const int Width);
      llvm::ConstantRange exhaustive(const llvm::ConstantRange &L, const llvm::ConstantRange &R,
			       Inst::Kind pred, const llvm::ConstantRange &Untrusted);

      void check(const llvm::ConstantRange &L, const llvm::ConstantRange &R, Inst::Kind pred,
		 double &FastBits, double &PreciseBits, int &Count, int &PreciseCount);

    public:

      static llvm::ConstantRange nextCR(const llvm::ConstantRange &CR);
      bool testFn(Inst::Kind pred);
    };

 namespace TestingUtil {

    void exhaustiveKBCRReduction(llvm::KnownBits &KB, llvm::ConstantRange &CR);

    template<int SetSize>
    std::bitset<SetSize> concretizeKB(const llvm::KnownBits &KB) {
      assert(SetSize == (1 << KB.getBitWidth()));

      std::bitset<SetSize> ResultSet(1);
      for (int Cnt = 0; Cnt < SetSize; Cnt++) {

	llvm::KnownBits ValueKB;
	auto Tmp = llvm::APInt(KB.getBitWidth(), Cnt);
	ValueKB.One = Tmp;
	ValueKB.Zero = ~Tmp;

	if (KnownBitsAnalysis::isConflictingKB(ValueKB, KB))
	  ResultSet[Cnt] = 0; // exclude this guy
      }

      return ResultSet;
    }

    template<int SetSize>
    std::bitset<SetSize> concretizeCR(const llvm::ConstantRange &CR) {
      assert(SetSize == (1 << CR.getBitWidth()));

      std::bitset<SetSize> ResultSet(0);
      if (CR.isFullSet()) {
	ResultSet.set();
	return ResultSet;
      }

      for (int Cnt = 0; Cnt < SetSize; Cnt++) {
	if (CR.contains(llvm::APInt(WIDTH, Cnt)))
	  ResultSet[Cnt] = 1; // include this guy
      }
      return ResultSet;
    }

    template<int SetSize>
    llvm::ConstantRange abstractizeCR(std::bitset<SetSize> &ResultSet) {
      llvm::ConstantRange ResultCR(WIDTH, false);
      for (unsigned I = 0; I < SetSize; I++) {
	if (ResultSet[I]) {
	  ResultCR = ResultCR.unionWith(llvm::APInt(WIDTH, I));
	}
      }

      return ResultCR;
    }

    template<int SetSize>
    llvm::KnownBits abstractizeKB(std::bitset<SetSize> &ResultSet) {
      llvm::KnownBits ResultKB(WIDTH);
      if (ResultSet.none())
	return ResultKB;

      llvm::APInt AndResult = llvm::APInt::getAllOnesValue(WIDTH);
      llvm::APInt OrResult = llvm::APInt::getNullValue(WIDTH);

      for (int I = 0; I < SetSize; I++) {
	if (ResultSet[I]) {
	  auto Tmp = llvm::APInt(WIDTH, I);
	  AndResult &= Tmp;
	  OrResult |= Tmp;
	}
      }

      ResultKB.One = AndResult;
      ResultKB.Zero = ~OrResult;

      assert(!ResultKB.hasConflict());

      return ResultKB;
    }

 } // ns TestingUtil
} // ns souper

#endif
