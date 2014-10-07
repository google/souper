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
#include "souper/Parser/Parser.h"

#include "hiredis.h"
#include <sstream>
#include <unordered_map>

STATISTIC(MemHitsInfer, "Number of internal cache hits for infer()");
STATISTIC(MemMissesInfer, "Number of internal cache misses for infer()");
STATISTIC(MemHitsIsValid, "Number of internal cache hits for isValid()");
STATISTIC(MemMissesIsValid, "Number of internal cache misses for isValid()");
STATISTIC(RedisHits, "Number of external cache hits");
STATISTIC(RedisMisses, "Number of external cache misses");
STATISTIC(TriviallyInvalid, "Number of trivially invalid expressions");

using namespace souper;
using namespace llvm;

namespace {

static cl::opt<bool> StaticProfile("souper-static-profile", cl::init(true),
    cl::desc("Static profiling of Souper optimizations (default=true)"));

class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

  std::error_code infer(const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, const InstOrigin &O,
                        InstContext &IC) {
    assert(LHS->Width == 1);
    std::error_code EC;
    std::vector<Inst *>Guesses { IC.getConst(APInt(1, true)),
                                 IC.getConst(APInt(1, false)) };
    for (auto I : Guesses) {
      // TODO: we can trivially synthesize an i1 undef by checking for validity
      // of both guesses
      InstMapping Mapping(LHS, I);
      CandidateExpr CE = GetCandidateExprForReplacement(PCs, Mapping);
      if (IsTriviallyInvalid(CE.E)) {
        ++TriviallyInvalid;
      } else {
        bool IsSat;
        EC = SMTSolver->isSatisfiable(BuildQuery(PCs, Mapping, 0), IsSat, 0, 0,
                                      Timeout);
        if (EC)
          return EC;
        if (!IsSat) {
          RHS = I;
          return EC;
        }
      }
    }
    RHS = 0;
    return EC;
  }

  std::error_code isValid(const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    std::string Query;
    if (Model && SMTSolver->supportsModels()) {
      std::vector<Inst *> ModelInsts;
      std::string Query = BuildQuery(PCs, Mapping, &ModelInsts);
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
      std::error_code EC = SMTSolver->isSatisfiable(BuildQuery(PCs, Mapping, 0),
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

  std::error_code infer(const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, const InstOrigin &O,
                        InstContext &IC) {
    std::string Repl = GetReplacementLHSString(PCs, LHS);
    const auto &ent = InferCache.find(Repl);
    if (ent == InferCache.end()) {
      ++MemMissesInfer;
      std::error_code EC = UnderlyingSolver->infer(PCs, LHS, RHS, O, IC);
      std::string RHSStr;
      if (!EC && RHS) {
        assert(RHS->K == Inst::Const);
        RHSStr = GetReplacementRHSString(RHS->Val);
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
        ParsedReplacement R = ParseReplacementRHS(IC, "<cache>", S, ES);
        if (ES != "")
          return std::make_error_code(std::errc::protocol_error);
        RHS = R.Mapping.RHS;
      }
      return ent->second.first;
    }
  }

  std::error_code isValid(const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    // TODO: add caching support for models.
    if (Model)
      return UnderlyingSolver->isValid(PCs, Mapping, IsValid, Model);

    std::string Repl = GetReplacementString(PCs, Mapping);
    const auto &ent = IsValidCache.find(Repl);
    if (ent == IsValidCache.end()) {
      ++MemMissesIsValid;
      std::error_code EC = UnderlyingSolver->isValid(PCs, Mapping, IsValid, 0);
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

class RedisCachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;
  redisContext *ctx;

public:
  RedisCachingSolver(std::unique_ptr<Solver> UnderlyingSolver)
      : UnderlyingSolver(std::move(UnderlyingSolver)) {

    // get these from cmd line?
    const char *hostname = "127.0.0.1";
    int port = 6379;

    struct timeval timeout = { 1, 500000 }; // 1.5 seconds
    ctx = redisConnectWithTimeout(hostname, port, timeout);
    if (!ctx) {
      llvm::report_fatal_error("Can't allocate redis context\n");
    }
    if (ctx->err) {
      llvm::report_fatal_error((std::string)"Redis connection error: " +
                               ctx->errstr + "\n");
    }
  }

  std::error_code infer(const std::vector<InstMapping> &PCs,
                        Inst *LHS, Inst *&RHS, const InstOrigin &O,
                        InstContext &IC) {
    std::string LHSStr = GetReplacementLHSString(PCs, LHS);

    if (StaticProfile) {
      Instruction *I = O.getInstruction();
      std::string Str;
      llvm::raw_string_ostream Loc(Str);
      I->getDebugLoc().print(I->getContext(), Loc);
      std::string HField = "sprofile " + Loc.str();
      redisReply *reply = (redisReply *)redisCommand(ctx, "HINCRBY %s %s 1",
          LHSStr.c_str(), HField.c_str());
      if (!reply) {
        llvm::report_fatal_error((std::string)"Redis error: " + ctx->errstr);
      }
      if (reply->type != REDIS_REPLY_INTEGER) {
        llvm::report_fatal_error(
            "Redis protocol error for static profile, didn't expect reply type "
            + std::to_string(reply->type));
      }
      freeReplyObject(reply);
    }

    redisReply *reply = (redisReply *)redisCommand(ctx, "HGET %s result",
                                                   LHSStr.c_str());
    if (!reply) {
      llvm::report_fatal_error((std::string)"Redis error: " + ctx->errstr);
    }
    if (reply->type == REDIS_REPLY_NIL) {
      freeReplyObject(reply);
      ++RedisMisses;
      std::error_code EC = UnderlyingSolver->infer(PCs, LHS, RHS, O, IC);
      std::string RHSStr;
      if (!EC && RHS) {
        assert(RHS->K == Inst::Const);
        RHSStr = GetReplacementRHSString(RHS->Val);
      }
      reply = (redisReply *)redisCommand(ctx, "HSET %s result %s",
                                         LHSStr.c_str(), RHSStr.c_str());
      if (!reply) {
        llvm::report_fatal_error((std::string)"Redis error: " + ctx->errstr);
      }
      if (reply->type != REDIS_REPLY_INTEGER) {
        llvm::report_fatal_error(
            "Redis protocol error for cache fill, didn't expect reply type " +
            std::to_string(reply->type));
      }
      freeReplyObject(reply);
      return EC;
    } else if (reply->type == REDIS_REPLY_STRING) {
      ++RedisHits;
      std::string ES;
      StringRef S = reply->str;
      if (S == "") {
        RHS = 0;
      } else {
        ParsedReplacement R = ParseReplacementRHS(IC, "<cache>", S, ES);
        if (ES != "")
          return std::make_error_code(std::errc::protocol_error);
        RHS = R.Mapping.RHS;
      }
      freeReplyObject(reply);
      return std::error_code();
    } else {
      llvm::report_fatal_error(
          "Redis protocol error for cache lookup, didn't expect reply type " +
          std::to_string(reply->type));
    }
  }

  std::error_code isValid(const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    // N.B. we decided that since the important clients have moved to infer(),
    // we'll no longer support Redis-based caching for isValid()
    return UnderlyingSolver->isValid(PCs, Mapping, IsValid, Model);
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

std::unique_ptr<Solver> createRedisCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver) {
  return std::unique_ptr<Solver>(
      new RedisCachingSolver(std::move(UnderlyingSolver)));
}

}
