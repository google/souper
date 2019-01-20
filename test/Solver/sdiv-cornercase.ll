; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %x) {
entry:
  %div = sdiv i32 %x, -1
  %a = icmp ne i32 %x, -2147483648
  %b = icmp ne i32 %div, %div, !expected !0
  %c = or i1 %a, %b, !expected !1
  ret i1 %c
}

!0 = !{i1 0}
!1 = !{i1 1}
