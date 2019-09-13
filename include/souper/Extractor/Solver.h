// Copyright 2014 The Souper Authors. All rights reserved.
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

#ifndef SOUPER_EXTRACTOR_SOLVER_H
#define SOUPER_EXTRACTOR_SOLVER_H

#include "llvm/ADT/APInt.h"
#include "souper/KVStore/KVStore.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "souper/Extractor/Candidates.h"
#include "souper/SMTLIB2/Solver.h"
#include <map>
#include <system_error>
#include <vector>

namespace souper {

class Solver {
public:
  virtual ~Solver();
  virtual std::error_code
  infer(const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
        Inst *LHS, Inst *&RHS, InstContext &IC) = 0;
  virtual std::error_code
  inferConst(const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
             Inst *LHS, Inst *&RHS, std::set<Inst *> &ConstSet,
             std::map<Inst *, llvm::APInt> &ResultMap, InstContext &IC) = 0;

  virtual std::error_code
  isValid(InstContext &IC, const BlockPCs &BPCs,
          const std::vector<InstMapping> &PCs,
          InstMapping Mapping, bool &IsValid,
          std::vector<std::pair<Inst *, llvm::APInt>> *Model) = 0;

  virtual std::string getName() = 0;

  virtual
  llvm::ConstantRange constantRange(const BlockPCs &BPCs,
                                    const std::vector<InstMapping> &PCs,
                                    Inst *LHS, InstContext &IC) = 0;
  virtual
  std::error_code negative(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &Negative,
                           InstContext &IC) = 0;
  virtual
  std::error_code knownBits(const BlockPCs &BPCs,
                            const std::vector<InstMapping> &PCs,
                            Inst *LHS, llvm::KnownBits &Known,
                            InstContext &IC) = 0;
  virtual
  std::error_code nonNegative(const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              Inst *LHS, bool &NonNegative,
                              InstContext &IC) = 0;
  virtual
  std::error_code powerTwo(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &PowerTwo,
                           InstContext &IC) = 0;
  virtual
  std::error_code nonZero(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          Inst *LHS, bool &NonZero,
                          InstContext &IC) = 0;
  virtual
  std::error_code signBits(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, unsigned &SignBits,
                           InstContext &IC) = 0;
  virtual
  std::error_code testDemandedBits(const BlockPCs &BPCs,
                                   const std::vector<InstMapping> &PCs,
                                   Inst *LHS,
                                   std::map<std::string, llvm::APInt> &DBitsVect,
                                   InstContext &IC) = 0;
};

std::unique_ptr<Solver> createBaseSolver(
    std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout);
std::unique_ptr<Solver> createMemCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver);
std::unique_ptr<Solver> createExternalCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver, KVStore *KV);

}

#endif  // SOUPER_EXTRACTOR_SOLVER_H
