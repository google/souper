// REQUIRES: solver
// RUN: SOUPER_SOLVER=%solver SOUPER_NO_EXTERNAL_CACHE=1 %sclang++ -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s
// CHECK: ret i1 false

#include <limits.h>

static bool divides_evenly(unsigned short prime, unsigned short n) {
  return (n <= 1 || n >= prime) ? false : (prime % n) == 0;
}

bool always_false(void) {
  for (int n = 0; n <= USHRT_MAX; ++n) {
    if (divides_evenly(29, n) ||
        divides_evenly(71, n) ||
        divides_evenly(113, n) ||
        divides_evenly(65267, n) ||
        divides_evenly(65381, n) ||
        divides_evenly(65497, n))
      return true;
  }
  return false;
}
