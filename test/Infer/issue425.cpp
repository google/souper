// REQUIRES: solver, synthesis

// RUN: SOUPER_SOLVER=%solver SOUPER_EXHAUSTIVE_SYNTHESIS=1 SOUPER_NO_EXTERNAL_CACHE=1 %sclang++ -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s
// CHECK-NOT: error

// Regression test for issue #425


template <typename a> void b(a, int);
struct c {
 enum d {};
};
struct e : c {
 d f();
 int g();
};
e::d e::f() {
 int h = g();
 long *i = new long[h];
 b(i, 0);
}
