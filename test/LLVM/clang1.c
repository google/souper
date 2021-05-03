// REQUIRES: solver

// RUN: %clang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST1 %s
// TEST1: %or = or i32 %b, %a

// RUN: %clang -O2 -S -o - %s -emit-llvm -mllvm -disable-all-peepholes | %FileCheck -check-prefix=TEST2 %s
// TEST2: %xor = xor i32 %a, %b
// TEST2-NEXT: %or = or i32 %xor, %a

// RUN: SOUPER_SOLVER=%solver SOUPER_NO_INFER=1 SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST3 %s
// TEST3: %or = or i32 %b, %a

// RUN: LLVM_DISABLE_PEEPHOLES=1 SOUPER_SOLVER=%solver SOUPER_NO_INFER=1 SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST4 %s
// TEST4: %xor = xor i32 %a, %b
// TEST4-NEXT: %or = or i32 %xor, %a

unsigned foo(unsigned a, unsigned b) {
  return (a ^ b) | a;
}
