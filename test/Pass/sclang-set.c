// REQUIRES: solver
// RUN: SOUPER_SOLVER=%solver SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s
// CHECK: ret i32 -268435456

#include <limits.h>
#include <stdint.h>

static uint32_t set_lowest_cleared_bit(uint32_t x) {
  x ^= (uint32_t)-1;
  x = x & (x - 1);
  x ^= (uint32_t)-1;
  return x;
}

uint32_t test(uint32_t x) {
  x = set_lowest_cleared_bit(x);
  x = set_lowest_cleared_bit(x);
  x = set_lowest_cleared_bit(x);
  x = set_lowest_cleared_bit(x);
  return x << 28;
}
