; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t 2> %t2 || true
; RUN: %FileCheck %s < %t2

define i1 @foo() {
entry:
  ; CHECK: instruction:
  ; CHECK-NEXT: icmp eq i32 0, 0
  ; CHECK-NEXT: unexpected simplification, wanted 0:
  ; CHECK-NEXT: ; Function: foo
  ; CHECK-NEXT: %0:i1 = eq 0:i32, 0:i32
  ; CHECK-NEXT: cand %0 1:i1
  %cmp = icmp eq i32 0, 0, !expected !0
  ret i1 %cmp
}

!0 = !{i1 0}
