// RUN: %clang -O2 -S -o - %s -emit-llvm | %FileCheck -check-prefix=TEST1 %s
// TEST1: %or = or i32 %b, %a

// RUN: %clang -O2 -S -o - %s -emit-llvm -mllvm -disable-peepholes | %FileCheck -check-prefix=TEST2 %s
// TEST2: %xor = xor i32 %b, %a
// TEST2-NEXT: %or = or i32 %xor, %a

unsigned foo(unsigned a, unsigned b) {
  return (a ^ b) | a;
}
