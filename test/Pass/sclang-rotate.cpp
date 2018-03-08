// REQUIRES: solver
// RUN: SOUPER_SOLVER=%solver SOUPER_NO_EXTERNAL_CACHE=1 %sclang++ -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s
// CHECK: ret i1 false

#include <stdint.h>

static uint8_t rotl(uint8_t x, int n) {
  return (x << n) | (x >> (8 - n));
}

static uint8_t rotr(uint8_t x, int n) {
  return (x >> n) | (x << (8 - n));
}

static int popcount(uint8_t x) {
  int count = 0;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  if (x & 1) count++; x >>= 1;
  return count;
}

bool always_false(uint8_t x, int n) {
  return popcount(rotr(x, n)) != popcount(rotl(x, n));
}
