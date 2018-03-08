// REQUIRES: solver
// RUN: SOUPER_SOLVER=%solver SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s
// CHECK: ret i64 0

#include <limits.h>
#include <stdint.h>

uint64_t test(uint64_t x) {
  x = x & (x - 1);
  x = x & (x - 1);
  x = x & (x - 1);
  x = x & (x - 1);
  return x << 60;
}
