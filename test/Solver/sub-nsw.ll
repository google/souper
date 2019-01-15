; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i64 %a) #0 {
entry:
  %nega = sub nsw i64 0, %a
  %cmp1 = icmp sge i64 %a, 0
  %cmp2 = icmp sge i64 %nega, 0
  %cmp = or i1 %cmp1, %cmp2, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
