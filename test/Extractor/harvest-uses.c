// REQUIRES: solver

// RUN: SOUPER_SOLVER=%solver SOUPER_HARVEST_USES=1 SOUPER_NO_EXTERNAL_CACHE=1 %sclang -O %s -S -o - -emit-llvm 2>&1 | %FileCheck %s

int a(int x);
int b(int x);
int func(int x) {
  int y = x * 11;
  int z = x * 17;
  if (y == 88)
    // CHECK: call i32 @a(i32 136)
    return a(z);
  else
    return b(z);
}
