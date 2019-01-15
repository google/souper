; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %x) #0 {
entry:
  %mul = mul nuw i32 %x, 4294967295
  %cmp1 = icmp eq i32 %mul, 0
  %cmp2 = icmp eq i32 %mul, 4294967295
  %cmp = or i1 %cmp1, %cmp2, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
