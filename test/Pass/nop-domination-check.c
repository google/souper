; REQUIRES: solver

; RUN: env SOUPER_NO_EXTERNAL_CACHE=1 SOUPER_INFER_NOP=1 SOUPER_SOLVER=%solver %sclang -O2 -c -mllvm -stats -o - %s 2>&1 | %FileCheck %s

; Reduced from SPEC CINT17 502.gcc_r/decNumber.c, this test will
; fail if domination check in Pass.cpp is disabled.

; CHECK-NOT: Instruction does not dominate all uses!
; CHECK: Number of failed replacement due to dominance check
; XFAIL: *

typedef struct {
  char a;
  short b[]
} c;
c d;
e, i, j, g, h;
*f;
k() {
  int a = j, b;
  if (i)
    *f = a;
  g = e + j - 1;
  if (g < 0) {
    b = -g % 3;
    if (b)
      b = 3 - b;
  } else
    b = g % 3;
  if (*d.b == 0 && j == 1 && l())
    b;
  else if (b)
    h = b;
  if (a)
    for (;;)
      ;
}
