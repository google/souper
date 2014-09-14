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

#include "llvm/ADT/Statistic.h"
#include "llvm/Support/ErrorHandling.h"
#include "souper/Extractor/Solver.h"

#include "hiredis.h"
#include <sstream>
#include <unordered_map>

STATISTIC(MemHits, "Number of internal cache hits");
STATISTIC(MemMisses, "Number of internal cache misses");
STATISTIC(RedisHits, "Number of external cache hits");
STATISTIC(RedisMisses, "Number of external cache misses");

using namespace souper;

namespace {

class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

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
  std::unordered_map<std::string, std::pair<std::error_code, bool>> Cache;

public:
  MemCachingSolver(std::unique_ptr<Solver> UnderlyingSolver)
      : UnderlyingSolver(std::move(UnderlyingSolver)) {}

  std::error_code isValid(const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    // TODO: add caching support for models.
    if (Model)
      return UnderlyingSolver->isValid(PCs, Mapping, IsValid, Model);

    std::string Repl = GetReplacementString(PCs, Mapping);
    const auto &ent = Cache.find(Repl);
    if (ent == Cache.end()) {
      ++MemMisses;
      std::error_code EC = UnderlyingSolver->isValid(PCs, Mapping, IsValid, 0);
      Cache.emplace (Repl, std::make_pair (EC, IsValid));
      return EC;
    } else {
      ++MemHits;
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

  std::error_code isValid(const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model) {
    // TODO: add caching support for models.
    if (Model)
      return UnderlyingSolver->isValid(PCs, Mapping, IsValid, Model);

    std::string Repl = GetReplacementString(PCs, Mapping);
    redisReply *reply = (redisReply *)redisCommand(ctx, "GET %s", Repl.c_str());
    if (!reply) {
      llvm::report_fatal_error((std::string)"Redis error: " + ctx->errstr);
    }

    if (reply->type == REDIS_REPLY_NIL) {
      freeReplyObject(reply);
      ++RedisMisses;
      std::error_code EC = UnderlyingSolver->isValid(PCs, Mapping, IsValid, 0);
      if (!EC) {
        reply = (redisReply *)redisCommand(ctx, "SET %s %d", Repl.c_str(),
            IsValid);
        if (!reply) {
          llvm::report_fatal_error((std::string)"Redis error: " + ctx->errstr);
        }
        if (reply->type != REDIS_REPLY_STATUS) {
          llvm::report_fatal_error(
              "Redis protocol error, didn't expect reply type " +
              std::to_string(reply->type));
        }
        freeReplyObject(reply);
      }
      return EC;
    } else if (reply->type == REDIS_REPLY_STRING) {
      std::istringstream(reply->str) >> IsValid;
      freeReplyObject(reply);
      ++RedisHits;
      return std::error_code();
    } else {
      llvm::report_fatal_error(
          "Redis protocol error, didn't expect reply type " +
          std::to_string(reply->type));
    }
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
