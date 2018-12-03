; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i64 %x, i64 %y) {
entry:
  %div = udiv i64 %x, %y
  %a = icmp ne i64 %y, 0
  %b = icmp ne i64 %div, %div, !expected !0
  %c = or i1 %a, %b, !expected !1
  ret i1 %c
}

!0 = !{i1 0}
!1 = !{i1 1}
