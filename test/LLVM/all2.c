// RUN: %clang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST1 %s
// TEST1: and i64 %x, 1

// RUN: %clang -O2 -S -o - %s -emit-llvm -mllvm -disable-peepholes | %FileCheck -check-prefix=TEST2 %s
// TEST2: shl i64 %x, 63
// TEST2-NEXT: lshr i64 %shl, 63

unsigned long foo(unsigned long x) {
  return ((x << 63) >> 63) + 1;
}
