#include <stdlib.h>
#include "hiredis.h"

static redisContext *ctx;
static const char *hostname = "127.0.0.1";
static const int port = 6379;

void _souper_profile (const char *repl)
{
  // FIXME this should be made thread safe
  if (!ctx) {
    struct timeval timeout = { 1, 500000 }; // 1.5 seconds
    ctx = redisConnectWithTimeout(hostname, port, timeout);
    if (!ctx) {
      fprintf(stderr, "FATAL: Can't allocate redis context\n");
      exit(-1);
    }
    if (ctx->err) {
      fprintf(stderr, "FATAL: Redis connection error: %s\n", ctx->errstr);
      exit(-1);
    }
  }

  redisReply *reply = (redisReply *)redisCommand(ctx, "INCR %s", repl);
  if (!reply) {
    fprintf(stderr, "FATAL: Redis error: %s\n", ctx->errstr);
    exit(-1);
  }
  if (reply->type != REDIS_REPLY_INTEGER) {
    fprintf(stderr, "FATAL: Redis protocol error, didn't expect reply type %d\n", reply->type);
    exit(-1);
  }
  freeReplyObject(reply);
}
