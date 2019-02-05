// REQUIRES: solver

// RUN: SOUPER_SOLVER=%solver SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O %s -o %t

// RUN: %t | %FileCheck %s
// CHECK: checksum = 3

// Regression test for issue #432

int printf(const char *, ...);
char a, c = 3;
main() {
  char b = c;
  int d = 8;
  a = (d >= 32 || b > 0) ? b : b << 8;
  printf("checksum = %d\n", a);
}
