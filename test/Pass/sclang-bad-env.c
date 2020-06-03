// RUN: SOUPER_BLARGH=1 %sclang -O3 %s -S -o - -emit-llvm 2>&1 | %FileCheck %s || true
// CHECK: unexpected Souper-related environment variable

void foo(void) {
}
