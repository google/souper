; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare void @sink(i1) nounwind readnone

; x^2 + 2 = y^3 has a unique positive solution

define void @bachet1(i32 %x, i32 %y) #0 {
entry:
  %xsqr = mul nuw i32 %x, %x
  %ysqr = mul nuw i32 %y, %y
  %ycub = mul nuw i32 %y, %ysqr
  %xsqrp2 = add nuw i32 %xsqr, 2
  %cmp1 = icmp eq i32 %xsqrp2, %ycub
  br i1 %cmp1, label %cont, label %out
cont:
  %check1 = icmp eq i32 %x, 5, !expected !1
  call void @sink(i1 %check1)
  %check2 = icmp eq i32 %y, 3, !expected !1
  call void @sink(i1 %check2)
  ret void
out:
  ret void
}

!1 = !{i1 1}
