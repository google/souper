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

#include "hiredis/hiredis.h"
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
                          InstMapping Mapping, bool &IsValid) {
    bool IsSat;
    std::error_code EC =
        SMTSolver->isSatisfiable(BuildQuery(PCs, Mapping), IsSat, Timeout);
    IsValid = !IsSat;
    return EC;
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
                          InstMapping Mapping, bool &IsValid) {
    std::string buf;
    llvm::raw_string_ostream OS(buf);
    souper::PrintReplacement(OS, PCs, Mapping);

    const auto &ent = Cache.find(OS.str());
    if (ent == Cache.end()) {
      ++MemMisses;
      std::error_code EC = UnderlyingSolver->isValid(PCs, Mapping, IsValid);
      Cache.emplace (OS.str(), std::make_pair (EC, IsValid));
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

    // FIXME -- get these from cmd line
    const char *hostname = "127.0.0.1";
    int port = 6379;

    struct timeval timeout = { 1, 500000 }; // 1.5 seconds
    ctx = redisConnectWithTimeout(hostname, port, timeout);
    if (ctx == NULL || ctx->err) {
      if (ctx) {
        llvm::report_fatal_error((std::string)"Redis connection error: " +
            ctx->errstr + "\n");
      } else {
        llvm::report_fatal_error("Can't allocate redis context\n");
      }
    }
  }

  std::error_code isValid(const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid) {
    std::string buf;
    llvm::raw_string_ostream OS(buf);
    souper::PrintReplacement(OS, PCs, Mapping);

    redisReply *reply = (redisReply *)redisCommand(ctx, "GET %s",
        OS.str().c_str());
    if (!reply) {
      llvm::report_fatal_error((std::string)"Redis error: " + ctx->errstr);
    }

    if (reply->type == REDIS_REPLY_NIL) {
      freeReplyObject(reply);
      ++RedisMisses;
      std::error_code EC = UnderlyingSolver->isValid(PCs, Mapping, IsValid);
      if (!EC) {
        reply = (redisReply *)redisCommand(ctx, "SET %s %d", OS.str().c_str(),
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
      std::string str = reply->str;
      std::istringstream(str) >> IsValid;
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
