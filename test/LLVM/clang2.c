// REQUIRES: solver

// RUN: %clang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST1 %s
// TEST1: and i64 %x, 1

// RUN: %clang -O2 -S -o - %s -emit-llvm -mllvm -disable-all-peepholes | %FileCheck -check-prefix=TEST2 %s
// TEST2: shl i64 %x, 63
// TEST2-NEXT: lshr i64 %shl, 63

// RUN: SOUPER_SOLVER=%solver SOUPER_NO_INFER=1 SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST3 %s
// TEST3: and i64 %x, 1

// RUN: LLVM_DISABLE_PEEPHOLES=1 SOUPER_SOLVER=%solver SOUPER_NO_INFER=1 SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST4 %s
// TEST4: shl i64 %x, 63
// TEST4-NEXT: lshr i64 %shl, 63

unsigned long foo(unsigned long x) {
  return ((x << 63) >> 63) + 1;
}
