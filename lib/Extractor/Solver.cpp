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
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"
#include "souper/Extractor/Solver.h"
#include "souper/Infer/AliveDriver.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/ExhaustiveSynthesis.h"
#include "souper/Infer/InstSynthesis.h"
#include "souper/KVStore/KVStore.h"
#include "souper/Parser/Parser.h"

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
static cl::opt<bool> InferNop("souper-infer-nop",
    cl::desc("Infer that the output is the same as an input value (default=false)"),
    cl::init(false));
static cl::opt<bool> StressNop("souper-stress-nop",
    cl::desc("stress-test big queries in nop synthesis by always performing all of the small queries (slow!) (default=false)"),
    cl::init(false));
static cl::opt<int>MaxNops("souper-max-nops",
    cl::desc("maximum number of values from the LHS to try to use as the RHS (default=20)"),
    cl::init(20));
static cl::opt<bool> InferInts("souper-infer-iN",
    cl::desc("Infer iN integers for N>1 (default=true)"),
    cl::init(true));
static cl::opt<bool> InferInsts("souper-infer-inst",
    cl::desc("Infer instructions (default=false)"),
    cl::init(false));
static cl::opt<bool> EnableExhaustiveSynthesis("souper-exhaustive-synthesis",
    cl::desc("Use exaustive search for instruction synthesis (default=false)"),
    cl::init(false));
static cl::opt<int> MaxLHSSize("souper-max-lhs-size",
    cl::desc("Max size of LHS (in bytes) to put in external cache (default=1024)"),
    cl::init(1024));

class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, InstContext &IC) override {
    std::error_code EC;

    /*
     * TODO: try to synthesize undef before synthesizing a concrete
     * integer
     */

    /*
     * Even though we have real integer synthesis below, first try to
     * guess a few constants that are likely to be cheap for the
     * backend to make
     */
    if (InferInts || LHS->Width == 1) {
      std::vector<Inst *>Guesses { IC.getConst(APInt(LHS->Width, 0)),
                                   IC.getConst(APInt(LHS->Width, 1)) };
      if (LHS->Width > 1)
        Guesses.emplace_back(IC.getConst(APInt(LHS->Width, -1)));
      for (auto I : Guesses) {
        InstMapping Mapping(LHS, I);

        if (UseAlive) {
          bool IsValid = isTransformationValid(Mapping.LHS, Mapping.RHS,
                                               PCs, IC);
          if (IsValid) {
            RHS = I;
            return std::error_code();
          }
          // TODO: Propagate errors from Alive backend, exit early for errors
        } else {
          std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, 0, /*Precondition=*/0);
          if (Query.empty())
            return std::make_error_code(std::errc::value_too_large);
          bool IsSat;
          EC = SMTSolver->isSatisfiable(Query, IsSat, 0, 0, Timeout);
          if (EC)
            return EC;
          if (!IsSat) {
            RHS = I;
            return EC;
          }
        }
      }
    }

    if (InferInts && SMTSolver->supportsModels() && LHS->Width > 1) {
      if (UseAlive) {
        //Try to synthesize a constant at the root
        Inst *C = IC.createVar(LHS->Width, "reservedconst_0");

        Inst *Ante = IC.getConst(llvm::APInt(1, true));
        for (auto PC : PCs ) {
          Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
          Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
        }

        AliveDriver Synthesizer(LHS, Ante, IC);
        auto ConstantMap = Synthesizer.synthesizeConstants(C);
        if (ConstantMap.find(C) != ConstantMap.end()) {
          RHS = IC.getConst(ConstantMap[C]);
          return std::error_code();
        }
        // TODO: Propagate errors from Alive backend, exit early for errors
      } else {
        Inst *C = IC.createVar(LHS->Width, "constant");
        std::map<Inst *, llvm::APInt> ResultMap;
        std::set<Inst*> ConstSet{C};
        ConstantSynthesis CS;
        EC = CS.synthesize(SMTSolver.get(), BPCs, PCs, InstMapping(LHS, C), ConstSet,
                           ResultMap, IC, /*MaxTries=*/1, Timeout);
        if (ResultMap.find(C) != ResultMap.end()) {
          RHS = IC.getConst(ResultMap[C]);
          return std::error_code();
        }
      }
    }

    // Do not do further synthesis if LHS is harvested from uses.
    if (LHS->HarvestKind == HarvestType::HarvestedFromUse)
      return EC;

    if (InferNop) {
      std::vector<Inst *> Guesses;
      findCands(LHS, Guesses, /*WidthMustMatch=*/true, /*FilterVars=*/false, MaxNops);

      Inst *Ante = IC.getConst(APInt(1, true));
      BlockPCs BPCsCopy;
      std::vector<InstMapping> PCsCopy;
      for (auto I : Guesses) {
        // separate sub-expressions by copying vars
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        Inst *Ne = IC.getInst(Inst::Ne, 1, {getInstCopy(LHS, IC, InstCache, BlockCache, 0, true),
              getInstCopy(I, IC, InstCache, BlockCache, 0, true)});
        Ante = IC.getInst(Inst::And, 1, {Ante, Ne});
        separateBlockPCs(BPCs, BPCsCopy, InstCache, BlockCache, IC, 0, true);
        separatePCs(PCs, PCsCopy, InstCache, BlockCache, IC, 0, true);
      }

      // (LHS != i_1) && (LHS != i_2) && ... && (LHS != i_n) == true
      InstMapping Mapping(Ante, IC.getConst(APInt(1, true)));
      std::string Query = BuildQuery(IC, BPCsCopy, PCsCopy, Mapping, 0,
                                     /*Precondition=*/0, /*Negate=*/true);
      if (Query.empty())
        return std::make_error_code(std::errc::value_too_large);
      bool BigQueryIsSat;
      EC = SMTSolver->isSatisfiable(Query, BigQueryIsSat, 0, 0, Timeout);
      if (EC)
        return EC;

      bool SmallQueryIsSat = true;
      if (StressNop || !BigQueryIsSat) {
        // find the nop
        for (auto I : Guesses) {
          InstMapping Mapping(LHS, I);
          std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, 0, /*Precondition=*/0);
          if (Query.empty())
            continue;
          EC = SMTSolver->isSatisfiable(Query, SmallQueryIsSat, 0, 0, Timeout);
          if (EC)
            return EC;
          if (!SmallQueryIsSat) {
            RHS = I;
            break;
          }
        }
      }

      if (!BigQueryIsSat && SmallQueryIsSat) {
        llvm::errs() << "*** oops ***\n";
        ReplacementContext C;
        llvm::errs() << GetReplacementLHSString(BPCs, PCs, LHS, C) << "\n";
        report_fatal_error("big query indicated a nop, but none was found");
      }
      if (BigQueryIsSat && !SmallQueryIsSat) {
        llvm::errs() << "*** oops ***\n";
        ReplacementContext C;
        llvm::errs() << GetReplacementLHSString(BPCs, PCs, LHS, C) << "\n";
        report_fatal_error("big query did not indicate a nop, but one was found");
      }

      if (!SmallQueryIsSat)
        return EC;
    }

    if(SMTSolver->supportsModels()) {
      if (EnableExhaustiveSynthesis) {
        ExhaustiveSynthesis ES;
        EC = ES.synthesize(SMTSolver.get(), BPCs, PCs, LHS, RHS, IC, Timeout);
        if (EC || RHS)
          return EC;
      } else if (InferInsts) {
        InstSynthesis IS;
        EC = IS.synthesize(SMTSolver.get(), BPCs, PCs, LHS, RHS, IC, Timeout);
        if (EC || RHS)
          return EC;
      }
    }

    RHS = 0;
    return EC;
  }

  std::error_code isValid(InstContext &IC, const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model)
  override {
    if (UseAlive) {
      IsValid = isTransformationValid(Mapping.LHS, Mapping.RHS, PCs, IC);
      return std::error_code();
    }
    std::string Query;
    if (Model && SMTSolver->supportsModels()) {
      std::vector<Inst *> ModelInsts;
      std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, &ModelInsts, /*Precondition=*/0);
      if (Query.empty())
        return std::make_error_code(std::errc::value_too_large);
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
      std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, 0, /*Precondition=*/0);
      if (Query.empty())
        return std::make_error_code(std::errc::value_too_large);
      bool IsSat;
      std::error_code EC = SMTSolver->isSatisfiable(Query, IsSat, 0, 0, Timeout);
      IsValid = !IsSat;
      return EC;
    }
  }

  std::error_code inferConst(const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS,
                             std::set<Inst *> &ConstSet,
                             std::map<Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC) override {
    ConstantSynthesis CS;
    return CS.synthesize(SMTSolver.get(), BPCs, PCs, InstMapping(LHS, RHS),
                         ConstSet, ResultMap, IC, 30, Timeout);
  }



  bool testKnown(const BlockPCs &BPCs,
		 const std::vector<InstMapping> &PCs,
		 APInt &Zeros, APInt &Ones, Inst *LHS,
		 InstContext &IC) {
    InstMapping Mapping(IC.getInst(Inst::And, LHS->Width,
                                   { IC.getConst(Zeros | Ones), LHS }),
                        IC.getConst(Ones));
    bool IsSat;
    auto Q = BuildQuery(IC, BPCs, PCs, Mapping, 0, /*Precondition=*/0);
    std::error_code EC = SMTSolver->isSatisfiable(Q, IsSat, 0, 0, Timeout);
    if (EC)
      return false;
    return !IsSat;
  }

  llvm::KnownBits findKnownBitsUsingSolver(const BlockPCs &BPCs,
                                           const std::vector<InstMapping> &PCs,
                                           Inst *LHS, InstContext &IC) override {
    unsigned W = LHS->Width;
    auto R = llvm::KnownBits(W);
    for (unsigned Pos = 0; Pos < W; Pos++) {
      APInt ZeroGuess = R.Zero | APInt::getOneBitSet(W, Pos);
      if (testKnown(BPCs, PCs, ZeroGuess, R.One, LHS, IC)) {
        R.Zero = ZeroGuess;
	continue;
      }
      APInt OneGuess = R.One | APInt::getOneBitSet(W, Pos);
      if (testKnown(BPCs, PCs, R.Zero, OneGuess, LHS, IC)) {
	R.One = OneGuess;
        continue;
      }
    }
    return R;
  }

  std::string getName() override {
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
                        Inst *LHS, Inst *&RHS, InstContext &IC) override {
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
  std::error_code inferConst(const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS,
                             std::set<Inst *> &ConstSet,
                             std::map<Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC) override {
    return UnderlyingSolver->inferConst(BPCs, PCs, LHS, RHS, ConstSet, ResultMap, IC);
  }

  std::error_code isValid(InstContext &IC, const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model)
    override {
    // TODO: add caching support for models.
    if (Model)
      return UnderlyingSolver->isValid(IC, BPCs, PCs, Mapping, IsValid, Model);

    std::string Repl = GetReplacementString(BPCs, PCs, Mapping);
    const auto &ent = IsValidCache.find(Repl);
    if (ent == IsValidCache.end()) {
      ++MemMissesIsValid;
      std::error_code EC = UnderlyingSolver->isValid(IC, BPCs, PCs,
                                                     Mapping, IsValid, 0);
      IsValidCache.emplace(Repl, std::make_pair(EC, IsValid));
      return EC;
    } else {
      ++MemHitsIsValid;
      IsValid = ent->second.second;
      return ent->second.first;
    }
  }

  llvm::KnownBits findKnownBitsUsingSolver(const BlockPCs &BPCs,
                                           const std::vector<InstMapping> &PCs,
                                           Inst *LHS, InstContext &IC) override {
    return UnderlyingSolver->findKnownBitsUsingSolver(BPCs, PCs, LHS, IC);
  }

  std::string getName() override {
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

  std::error_code inferConst(const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS,
                             std::set<Inst *> &ConstSet,
                             std::map<Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC) override {
    return UnderlyingSolver->inferConst(BPCs, PCs, LHS, RHS, ConstSet, ResultMap, IC);
  }


  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, InstContext &IC) override {
    ReplacementContext Context;
    std::string LHSStr = GetReplacementLHSString(BPCs, PCs, LHS, Context);
    if (LHSStr.length() > MaxLHSSize)
      return std::make_error_code(std::errc::value_too_large);
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
        RHS = 0;
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

  std::error_code isValid(InstContext &IC, const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model)
  override {
    // N.B. we decided that since the important clients have moved to infer(),
    // we'll no longer support external caching for isValid()
    return UnderlyingSolver->isValid(IC, BPCs, PCs, Mapping, IsValid, Model);
  }

  llvm::KnownBits findKnownBitsUsingSolver(const BlockPCs &BPCs,
                                           const std::vector<InstMapping> &PCs,
                                           Inst *LHS, InstContext &IC) override {
    return UnderlyingSolver->findKnownBitsUsingSolver(BPCs, PCs, LHS, IC);
  }

  std::string getName() override {
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
