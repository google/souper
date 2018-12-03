; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; x^2 - 45 = y^3 has no positive solution

define i1 @bachet2(i32 %x, i32 %y) #0 {
entry:
  %xsqr = mul nuw i32 %x, %x
  %ysqr = mul nuw i32 %y, %y
  %ycub = mul nuw i32 %y, %ysqr
  %xsqrm45 = sub nuw i32 %xsqr, 45
  %cmp1 = icmp eq i32 %xsqrm45, %ycub, !expected !0
  ret i1 %cmp1
}

!0 = !{i1 0}
