; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t 2> %t2 || true
; RUN: %FileCheck %s < %t2

define void @foo(i32 %p, i32 %q) {
entry:
  ; CHECK: instruction:
  ; CHECK-NEXT: icmp eq i32 %
  ; CHECK-NEXT: expected simplification, none found
  %cmp = icmp eq i32 %p, %q, !expected !1
  ret void
}

!1 = !{i1 1}
