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

#define DEBUG_TYPE "souper"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "souper/Extractor/Solver.h"
#include "souper/KVStore/KVStore.h"
#include "souper/Parser/Parser.h"

#include <sstream>
#include <unordered_map>

STATISTIC(MemHitsInfer, "Number of internal cache hits for infer()");
STATISTIC(MemMissesInfer, "Number of internal cache misses for infer()");
STATISTIC(MemHitsIsValid, "Number of internal cache hits for isValid()");
STATISTIC(MemMissesIsValid, "Number of internal cache misses for isValid()");
STATISTIC(ExternalHits, "Number of external cache hits");
STATISTIC(ExternalMisses, "Number of external cache misses");

using namespace souper;
using namespace llvm;

namespace {

static cl::opt<bool> NoInfer("souper-no-infer",
    cl::desc("Populate the external cache, but don't infer replacements (default=false)"),
    cl::init(false));

static cl::opt<bool> InferI1("souper-infer-i1",
    cl::desc("Infer Boolean values (default=true)"),
    cl::init(true));

static cl::opt<bool> InferNop("souper-infer-nop",
    cl::desc("Infer nops (default=false)"),
    cl::init(false));

static cl::opt<bool> InferUnary("souper-infer-unary",
    cl::desc("Infer unary instructions (default=false)"),
    cl::init(false));

class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

private:
  void getInputs(Inst *I, std::set<Inst *> &Inputs) {
    if (Inputs.insert(I).second)
      for (auto Op : I->Ops)
        getInputs(Op, Inputs);
  }

  int costHelper(Inst *I, std::set<Inst *> &Visited) {
    if (!Visited.insert(I).second)
      return 0;
    int Cost = 1;
    for (auto Op : I->Ops)
      Cost += costHelper(Op, Visited);
    return Cost;
  }

  int cost(Inst *I) {
    std::set<Inst *> Visited;
    return costHelper(I, Visited);
  }

  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, InstContext &IC) {
    std::error_code EC;

    if (LHS->Width == 1 && InferI1) {
      std::vector<Inst *>Guesses { IC.getConst(APInt(1, true)),
                                   IC.getConst(APInt(1, false)) };
      for (auto G : Guesses) {
        // TODO: we can trivially synthesize an i1 undef by checking for
        // validity of both guesses
        InstMapping Mapping(LHS, G);
        bool IsSat;
        EC = SMTSolver->isSatisfiable(BuildQuery(BPCs, PCs, Mapping, 0),
                                      IsSat, 0, 0, Timeout);
        if (EC)
          return EC;
        if (!IsSat) {
          RHS = G;
          return EC;
        }
      }
    }

    // TODO: constant synthesis goes here

    std::set<Inst *> Inputs;
    if (InferNop || InferUnary)
      for (auto Op : LHS->Ops)
        getInputs(Op, Inputs);

    if (InferNop) {
      for (auto I : Inputs) {
        if (I->Width == 1 &&
            (I->K == Inst::Const || I->K == Inst::UntypedConst))
          continue;
        if (LHS->Width != I->Width)
          continue;
        InstMapping Mapping(LHS, I);
        bool IsSat;
        EC = SMTSolver->isSatisfiable(BuildQuery(BPCs, PCs, Mapping, 0), IsSat,
                                      0, 0, Timeout);
        if (EC)
          return EC;
        if (!IsSat) {
          RHS = I;
          return EC;
        }
      }
    }

    if (InferUnary) {
      int LHSCost = cost(LHS);
      for (auto I : Inputs) {
        if (I->Width == 1 &&
            (I->K == Inst::Const || I->K == Inst::UntypedConst))
          continue;
        std::vector<Inst *> Guesses;
        if (LHS->Width > I->Width) {
          Guesses.push_back(IC.getInst(Inst::SExt, LHS->Width, {I}));
          Guesses.push_back(IC.getInst(Inst::ZExt, LHS->Width, {I}));
        } else if (LHS->Width < I->Width) {
          Guesses.push_back(IC.getInst(Inst::Trunc, LHS->Width, {I}));
        } else {
          Guesses.push_back(IC.getInst(Inst::Xor, LHS->Width,
              { IC.getConst(APInt(I->Width, -1)), I }));
          Guesses.push_back(IC.getInst(Inst::Sub, LHS->Width,
              { IC.getConst(APInt(I->Width, 0)), I }));
        }
        for (auto G : Guesses) {
          if (LHSCost - cost(G) < 1)
            continue;
          InstMapping Mapping(LHS, G);
          bool IsSat;
          EC = SMTSolver->isSatisfiable(BuildQuery(BPCs, PCs, Mapping, 0),
                                        IsSat, 0, 0, Timeout);
          if (EC)
            return EC;
          if (!IsSat) {
            RHS = G;
            return EC;
          }
        }
      }
    }

    RHS = 0;
    return EC;
  }

  std::error_code isValid(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    std::string Query;
    if (Model && SMTSolver->supportsModels()) {
      std::vector<Inst *> ModelInsts;
      std::string Query = BuildQuery(BPCs, PCs, Mapping, &ModelInsts);
      bool IsSat;
      std::vector<llvm::APInt> ModelVals;
      std::error_code EC = SMTSolver->isSatisfiable(
          Query, IsSat, ModelInsts.size(), &ModelVals, Timeout);
      if (!EC) {
        if (IsSat) {
          for (unsigned I = 0; I != ModelInsts.size(); ++I) {
            Model->push_back(std::make_pair(ModelInsts[I], ModelVals[I]));
          }
        }
        IsValid = !IsSat;
      }
      return EC;
    } else {
      bool IsSat;
      std::error_code EC =
        SMTSolver->isSatisfiable(BuildQuery(BPCs, PCs, Mapping, 0),
                                 IsSat, 0, 0, Timeout);
      IsValid = !IsSat;
      return EC;
    }
  }

  std::string getName() {
    return SMTSolver->getName();
  }
};

class MemCachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;
  std::unordered_map<std::string, std::pair<std::error_code, bool>> IsValidCache;
  std::unordered_map<std::string, std::pair<std::error_code, std::string>>
    InferCache;

public:
  MemCachingSolver(std::unique_ptr<Solver> UnderlyingSolver)
      : UnderlyingSolver(std::move(UnderlyingSolver)) {}

  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, InstContext &IC) {
    ReplacementContext Context;
    std::string Repl = GetReplacementLHSString(BPCs, PCs, LHS, Context);
    const auto &ent = InferCache.find(Repl);
    if (ent == InferCache.end()) {
      ++MemMissesInfer;
      std::error_code EC = UnderlyingSolver->infer(BPCs, PCs, LHS, RHS, IC);
      std::string RHSStr;
      if (!EC && RHS) {
        RHSStr = GetReplacementRHSString(RHS, Context);
      }
      InferCache.emplace(Repl, std::make_pair(EC, RHSStr));
      return EC;
    } else {
      ++MemHitsInfer;
      std::string ES;
      StringRef S = ent->second.second;
      if (S == "") {
        RHS = 0;
      } else {
        ParsedReplacement R = ParseReplacementRHS(IC, "<cache>", S, Context, ES);
        if (ES != "")
          return std::make_error_code(std::errc::protocol_error);
        RHS = R.Mapping.RHS;
      }
      return ent->second.first;
    }
  }

  std::error_code isValid(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    // TODO: add caching support for models.
    if (Model)
      return UnderlyingSolver->isValid(BPCs, PCs, Mapping, IsValid, Model);

    std::string Repl = GetReplacementString(BPCs, PCs, Mapping);
    const auto &ent = IsValidCache.find(Repl);
    if (ent == IsValidCache.end()) {
      ++MemMissesIsValid;
      std::error_code EC = UnderlyingSolver->isValid(BPCs, PCs,
                                                     Mapping, IsValid, 0);
      IsValidCache.emplace(Repl, std::make_pair(EC, IsValid));
      return EC;
    } else {
      ++MemHitsIsValid;
      IsValid = ent->second.second;
      return ent->second.first;
    }
  }

  std::string getName() {
    return UnderlyingSolver->getName() + " + internal cache";
  }

};

class ExternalCachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;
  KVStore *KV;

public:
  ExternalCachingSolver(std::unique_ptr<Solver> UnderlyingSolver, KVStore *KV)
      : UnderlyingSolver(std::move(UnderlyingSolver)), KV(KV) {
  }

  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, InstContext &IC) {
    ReplacementContext Context;
    std::string LHSStr = GetReplacementLHSString(BPCs, PCs, LHS, Context);
    std::string S;
    if (KV->hGet(LHSStr, "result", S)) {
      ++ExternalHits;
      if (S == "") {
        RHS = 0;
      } else {
        std::string ES;
        ParsedReplacement R = ParseReplacementRHS(IC, "<cache>", S, Context, ES);
        if (ES != "")
          return std::make_error_code(std::errc::protocol_error);
        RHS = R.Mapping.RHS;
      }
      return std::error_code();
    } else {
      ++ExternalMisses;
      if (NoInfer) {
        KV->hSet(LHSStr, "result", "");
        return std::error_code();
      }
      std::error_code EC = UnderlyingSolver->infer(BPCs, PCs, LHS, RHS, IC);
      std::string RHSStr;
      if (!EC && RHS) {
        RHSStr = GetReplacementRHSString(RHS, Context);
      }
      KV->hSet(LHSStr, "result", RHSStr);
      return EC;
    }
  }

  std::error_code isValid(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    // N.B. we decided that since the important clients have moved to infer(),
    // we'll no longer support external caching for isValid()
    return UnderlyingSolver->isValid(BPCs, PCs, Mapping, IsValid, Model);
  }

  std::string getName() {
    return UnderlyingSolver->getName() + " + external cache";
  }

};

}

namespace souper {

Solver::~Solver() {}

std::unique_ptr<Solver> createBaseSolver(
    std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout) {
  return std::unique_ptr<Solver>(new BaseSolver(std::move(SMTSolver), Timeout));
}

std::unique_ptr<Solver> createMemCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver) {
  return std::unique_ptr<Solver>(
      new MemCachingSolver(std::move(UnderlyingSolver)));
}

std::unique_ptr<Solver> createExternalCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver, KVStore *KV) {
  return std::unique_ptr<Solver>(
      new ExternalCachingSolver(std::move(UnderlyingSolver), KV));
}

}
