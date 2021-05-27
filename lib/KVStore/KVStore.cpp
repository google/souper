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

#include "souper/KVStore/KVStore.h"
#include "souper/KVStore/KVSocket.h"

#include "llvm/Support/CommandLine.h"
#include "hiredis.h"

using namespace llvm;
using namespace souper;

static cl::opt<unsigned> RedisPort("souper-redis-port", cl::init(6379),
    cl::desc("Redis server port (default=6379)"));
static cl::opt<bool> UnixSocket("souper-external-cache-unix", cl::init(false),
    cl::desc("Talk to the cache using UNIX domain sockets (default=false)"));

static const int MAX_RETRIES = 5;

namespace souper {

class KVStore::KVImpl {
  redisContext *Ctx = nullptr;
  int retries = 0;
public:
  KVImpl();
  ~KVImpl();
  void hIncrBy(llvm::StringRef Key, llvm::StringRef Field, int Incr);
  bool hGet(llvm::StringRef Key, llvm::StringRef Field, std::string &Value);
  void hSet(llvm::StringRef Key, llvm::StringRef Field, llvm::StringRef Value);
  void connect();
};

void KVStore::KVImpl::connect() {
  if (Ctx)
    redisFree(Ctx);
  if (++retries > 5)
    llvm::report_fatal_error("Too many Redis retries\n");
  if (UnixSocket) {
    Ctx = redisConnectUnix(SocketPath);
    if (!Ctx)
      llvm::report_fatal_error("Can't allocate redis context\n");
    if (Ctx->err)
      llvm::report_fatal_error((llvm::StringRef)"Redis UNIX connection error: " +
			       Ctx->errstr + "\n");
  } else {
    // TODO: support connecting to other machines
    const char *hostname = "127.0.0.1";
    Ctx = redisConnect(hostname, RedisPort);
    if (!Ctx)
      llvm::report_fatal_error("Can't allocate redis context\n");
    if (Ctx->err)
      llvm::report_fatal_error((llvm::StringRef)"Redis TCP connection error: " +
			       Ctx->errstr + "\n");
    if (redisEnableKeepAlive(Ctx) != REDIS_OK)
      llvm::report_fatal_error("Can't enable redis keepalive\n");
  }
}

KVStore::KVImpl::KVImpl() {
  connect();
}

KVStore::KVImpl::~KVImpl() {
  redisFree(Ctx);
}

void KVStore::KVImpl::hIncrBy(llvm::StringRef Key, llvm::StringRef Field,
                              int Incr) {
 again:
  redisReply *reply = (redisReply *)redisCommand(Ctx, "HINCRBY %s %s 1",
                                                 Key.data(), Field.data());
  if (!reply || Ctx->err) {
    llvm::errs() << (llvm::StringRef)"Redis error: " + Ctx->errstr;
    connect();
    goto again;
  }
  if (reply->type != REDIS_REPLY_INTEGER)
    llvm::report_fatal_error(
        "Redis protocol error for static profile, didn't expect reply type "
        + std::to_string(reply->type));
  freeReplyObject(reply);
}

bool KVStore::KVImpl::hGet(llvm::StringRef Key, llvm::StringRef Field,
                           std::string &Value) {
 again:
  redisReply *reply = (redisReply *)redisCommand(Ctx, "HGET %s %s", Key.data(),
                                                 Field.data());
  if (!reply || Ctx->err) {
    llvm::errs() << (llvm::StringRef)"Redis error: " + Ctx->errstr;
    connect();
    goto again;
  }
  if (reply->type == REDIS_REPLY_NIL) {
    freeReplyObject(reply);
    return false;
  } else if (reply->type == REDIS_REPLY_STRING) {
    Value = reply->str;
    freeReplyObject(reply);
    return true;
  } else {
    llvm::report_fatal_error(
        "Redis protocol error for cache lookup, didn't expect reply type " +
        std::to_string(reply->type));
  }
}

void KVStore::KVImpl::hSet(llvm::StringRef Key, llvm::StringRef Field,
                              llvm::StringRef Value) {
  redisReply *reply = (redisReply *)redisCommand(Ctx, "HSET %s %s %s",
      Key.data(), Field.data(), Value.data());
  if (!reply || Ctx->err)
    llvm::report_fatal_error((llvm::StringRef)"Redis error: " + Ctx->errstr);
  if (reply->type != REDIS_REPLY_INTEGER) {
    llvm::report_fatal_error(
        "Redis protocol error for cache fill, didn't expect reply type " +
        std::to_string(reply->type));
  }
  freeReplyObject(reply);
}

KVStore::KVStore() : Impl (new KVImpl) {}

KVStore::~KVStore() {}

void KVStore::hIncrBy(llvm::StringRef Key, llvm::StringRef Field, int Incr) {
  Impl->hIncrBy(Key, Field, Incr);
}

bool KVStore::hGet(llvm::StringRef Key, llvm::StringRef Field,
                   std::string &Value) {
  return Impl->hGet(Key, Field, Value);
}

void KVStore::hSet(llvm::StringRef Key, llvm::StringRef Field,
                   llvm::StringRef Value) {
  Impl->hSet(Key, Field, Value);
}

}
