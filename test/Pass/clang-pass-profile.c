// REQUIRES: solver

// RUN: env SOUPER_NO_EXTERNAL_CACHE=1 SOUPER_DYNAMIC_PROFILE=1 SOUPER_SOLVER=%solver %sclang -O %s -o %t

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 0 | %FileCheck %s -check-prefix=ARG0
// ARG0: count = 0

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 1 | %FileCheck %s -check-prefix=ARG1
// ARG1: count = 1

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 2 | %FileCheck %s -check-prefix=ARG2
// ARG2: count = 100

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 3 | %FileCheck %s -check-prefix=ARG3
// ARG3: count = 101

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 4 | %FileCheck %s -check-prefix=ARG4
// ARG4: count = 10000

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 5 | %FileCheck %s -check-prefix=ARG5
// ARG5: count = 10001

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 6 | %FileCheck %s -check-prefix=ARG6
// ARG6: count = 10100

// RUN: env SOUPER_PROFILE_TO_STDOUT=1 %t 7 | %FileCheck %s -check-prefix=ARG7
// ARG7: count = 10101

#include <stdlib.h>

volatile unsigned opaque;

// this is fragile: any code where Souper can find an optimization that LLVM
// misses
void souper_opt(void) {
  unsigned a = opaque & 0xf;
  if (__builtin_popcount(a) + __builtin_popcount(~a) == 32)
      ++opaque;
}

int main(int argc, char *argv[]) {
  if (argc != 2)
    abort();
  long arg = strtol(argv[1], 0, 10);
  if (arg < 0 || arg > 7)
    abort();

  if (arg & 1)
    souper_opt();
  int i;
  for (i = 0; i < 100; ++i) {
    if (arg & 2)
      souper_opt();
    int j;
    for (j = 0; j < 100; ++j) {
      if (arg & 4)
        souper_opt();
    }
  }
  return 0;
}
