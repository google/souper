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

static struct rec_t {
  const char *repl;
  int64_t *cntp;
  struct rec_t *next;
} *recs;

static void _souper_atexit_handler(void)
{
  redisContext *ctx = connect();
  struct rec_t *recp;
  for (recp = recs; recp; recp = recp->next) {
    redisReply *reply = (redisReply *)redisCommand(ctx, "INCRBY %s %" PRId64 "", recp->repl, *recp->cntp);
    ensure_integer_reply(reply, ctx);
  }
}

static int init;

void _souper_profile_register(const char *repl, int64_t *cntp)
{
  // FIXME this should be made thread safe

  if (!init) {
    init = 1;
    if (atexit(_souper_atexit_handler) != 0) {
      fprintf(stderr, "FATAL: Can't install atexit handler\n");
      exit(-1);
    }
  }

  struct rec_t *rec = malloc(sizeof(struct rec_t));
  if (!rec) {
    fprintf(stderr, "FATAL: Allocation failure\n");
    exit(-1);
  }

  rec->repl = repl;
  rec->cntp = cntp;
  rec->next = recs;
  recs = rec;
}
