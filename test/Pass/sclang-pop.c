// REQUIRES: solver
// RUN: SOUPER_SOLVER=%solver SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s
// CHECK: ret i32 16

#include <limits.h>
#include <stdint.h>

static int pop16(uint16_t x) {
  return
    ((x & 1) != 0) +
    ((x & 2) != 0) +
    ((x & 4) != 0) +
    ((x & 8) != 0) +
    ((x & 16) != 0) +
    ((x & 32) != 0) +
    ((x & 64) != 0) +
    ((x & 128) != 0) +
    ((x & 256) != 0) +
    ((x & 512) != 0) +
    ((x & 1024) != 0) +
    ((x & 2048) != 0) +
    ((x & 4096) != 0) +
    ((x & 8192) != 0) +
    ((x & 16384) != 0) +
    ((x & 32768) != 0);
}

int return_16(uint16_t x) {
  return pop16(x) + pop16(x ^ 65535);
}
