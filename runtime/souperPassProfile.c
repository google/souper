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

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "hiredis.h"

static const char *hostname = "127.0.0.1";
static const int port = 6379;

static redisContext *connect(void)
{
  struct timeval timeout = { 1, 500000 }; // 1.5 seconds
  redisContext *ctx = redisConnectWithTimeout(hostname, port, timeout);
  if (!ctx) {
    fprintf(stderr, "FATAL: Can't allocate redis context\n");
    _exit(-1);
  }
  if (ctx->err) {
    fprintf(stderr, "FATAL: Redis connection error: %s\n", ctx->errstr);
    _exit(-1);
  }
  return ctx;
}

static void ensure_integer_reply(redisReply *reply, redisContext *ctx)
{
  if (!reply || ctx->err) {
    fprintf(stderr, "FATAL: Redis error: %s\n", ctx->errstr);
    _exit(-1);
  }
  if (reply->type != REDIS_REPLY_INTEGER) {
    fprintf(stderr, "FATAL: Redis protocol error, didn't expect reply type %d\n",
            reply->type);
    _exit(-1);
  }
  freeReplyObject(reply);
}

static int to_stdout;
static int to_stderr;

static struct rec_t {
  const char *repl;
  const char *field;
  int64_t *cntp;
  struct rec_t *next;
} *recs;

static void _souper_atexit_handler(void)
{
  redisContext *ctx;
  if (!to_stdout && !to_stderr)
    ctx = connect();
  struct rec_t *rec;
  for (rec = recs; rec; rec = rec->next) {
    int64_t inc = *rec->cntp;
    if (to_stdout || to_stderr) {
      FILE *f = to_stdout ? stdout : stderr;
      fprintf(f, "Souper profile key = '%s'\n", rec->repl);
      fprintf(f, "  field = '%s'\n", rec->field);
      fprintf(f, "  count = %" PRId64 "\n\n", inc);
    } else if (inc > 0) {
      redisReply *reply = (redisReply *)redisCommand(ctx,
          "HINCRBY %s %s %" PRId64 "", rec->repl, rec->field, inc);
      ensure_integer_reply(reply, ctx);
    }
  }
}

static volatile int lock;
static int init;

void _souper_profile_register(const char *repl, const char *field, int64_t *cntp)
{
  while (__atomic_exchange_n(&lock, 1, __ATOMIC_ACQUIRE));

  if (!init) {
    init = 1;
    if (atexit(_souper_atexit_handler) != 0) {
      fprintf(stderr, "FATAL: Can't install atexit handler\n");
      exit(-1);
    }
    if (getenv("SOUPER_PROFILE_TO_STDOUT"))
      to_stdout = 1;
    if (getenv("SOUPER_PROFILE_TO_STDERR"))
      to_stderr = 1;
  }

  struct rec_t *rec = malloc(sizeof(struct rec_t));
  if (!rec) {
    fprintf(stderr, "FATAL: Allocation failure\n");
    exit(-1);
  }

  rec->repl = repl;
  rec->field = field;
  rec->cntp = cntp;
  rec->next = recs;
  recs = rec;

  __atomic_exchange_n(&lock, 0, __ATOMIC_RELEASE);
}
