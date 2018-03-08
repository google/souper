// REQUIRES: solver

// RUN: %clang -Xclang -load -Xclang %pass -O2 -mllvm %solver -emit-llvm -S -o - %s -mllvm -print-after-all 2>&1 | %FileCheck %s

// Check that peephole passes are registered at least twice.

// CHECK: IR Dump After Souper super-optimizer pass
// CHECK: IR Dump After Souper super-optimizer pass
void foo() {}
