#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "hiredis.h"

static redisContext *ctx;
static const char *hostname = "127.0.0.1";
static const int port = 6379;

static void connect(void)
{
  struct timeval timeout = { 1, 500000 }; // 1.5 seconds
  ctx = redisConnectWithTimeout(hostname, port, timeout);
  if (!ctx) {
    fprintf(stderr, "FATAL: Can't allocate redis context\n");
    _exit(-1);
  }
  if (ctx->err) {
    fprintf(stderr, "FATAL: Redis connection error: %s\n", ctx->errstr);
    _exit(-1);
  }
}

static void check_integer_reply(redisReply *reply)
{
  if (!reply) {
    fprintf(stderr, "FATAL: Redis error: %s\n", ctx->errstr);
    _exit(-1);
  }
  if (reply->type != REDIS_REPLY_INTEGER) {
    fprintf(stderr, "FATAL: Redis protocol error, didn't expect reply type %d\n", reply->type);
    _exit(-1);
  }
  freeReplyObject(reply);
}

void _souper_profile(const char *repl)
{
  // FIXME this should be made thread safe
  if (!ctx)
    connect();

  redisReply *reply = (redisReply *)redisCommand(ctx, "INCR %s", repl);
  check_integer_reply(reply);
}

static int nrecs;
static int init;

static struct rec {
  const char *repl;
  int64_t cnt;
} *recs;

static void _souper_atexit_handler(void)
{
  connect();
  int i;
  for (i=0; i<nrecs; ++i) {
    if (!recs[i].repl)
      continue;
    redisReply *reply = (redisReply *)redisCommand(ctx, "INCRBY %s %" PRId64 "", recs[i].repl, recs[i].cnt);
    check_integer_reply(reply);
  }
  redisFree(ctx);
}

void _souper_profile_lazy(const char *repl, uint32_t index)
{
  // FIXME this should be made thread safe
  if (!init) {
    init = 1;
    if (atexit(_souper_atexit_handler) != 0) {
      fprintf(stderr, "FATAL: Can't install atexit handler\n");
      exit(-1);
    }
  }

  if (index >= nrecs) {
    recs = (struct rec *)realloc(recs, sizeof(struct rec) * (1 + index));
    if (!recs) {
      fprintf(stderr, "FATAL: realloc() failed\n");
      exit(-1);
    }
    while (nrecs <= index) {
      recs[nrecs].repl = 0;
      recs[nrecs].cnt = 0;
      ++nrecs;
    }
  }

  recs[index].repl = repl;
  ++recs[index].cnt;
}
