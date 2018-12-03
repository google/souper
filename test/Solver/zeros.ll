; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i32 @llvm.ctlz.i32(i32) nounwind readnone
declare i32 @llvm.cttz.i32(i32) nounwind readnone
declare i32 @llvm.ctpop.i32(i32) nounwind readnone
declare void @sink(i1) nounwind readnone

define void @foo(i32 %x) {
entry:
  %cmp0 = icmp eq i32 %x, 0
  br i1 %cmp0, label %zero, label %nonzero

zero:
  %noones = call i32 @llvm.ctpop.i32(i32 %x)
  %cmp1 = icmp eq i32 %noones, 0, !expected !1

  %allZeros1 = call i32 @llvm.ctlz.i32(i32 %x)
  %cmp2 = icmp eq i32 %allZeros1, 32, !expected !1

  %allZeros2 = call i32 @llvm.cttz.i32(i32 %x)
  %cmp3 = icmp eq i32 %allZeros2, 32, !expected !1

  call void @sink(i1 %cmp1)
  call void @sink(i1 %cmp2)
  call void @sink(i1 %cmp3)

  br label %out

nonzero:
  %ones = call i32 @llvm.ctpop.i32(i32 %x)
  %cmp4 = icmp sgt i32 %ones, 0, !expected !1
  %cmp5 = icmp sle i32 %ones, 32, !expected !1
  %zeros = sub i32 32, %ones

  %leadZeros = call i32 @llvm.ctlz.i32(i32 %x)
  %cmp6 = icmp sge i32 %leadZeros, 0, !expected !1
  %cmp7 = icmp slt i32 %leadZeros, 32, !expected !1

  %trailZeros = call i32 @llvm.cttz.i32(i32 %x)
  %cmp8 = icmp sge i32 %trailZeros, 0, !expected !1
  %cmp9 = icmp slt i32 %trailZeros, 32, !expected !1

  %ltZeros = add i32 %leadZeros, %trailZeros
  %cmp10 = icmp sge i32 %ltZeros, 0, !expected !1
  %cmp11 = icmp slt i32 %ltZeros, 32, !expected !1

  %cmp12 = icmp slt i32 %zeros, %ltZeros, !expected !0

  call void @sink(i1 %cmp4)
  call void @sink(i1 %cmp5)
  call void @sink(i1 %cmp6)
  call void @sink(i1 %cmp7)
  call void @sink(i1 %cmp8)
  call void @sink(i1 %cmp9)
  call void @sink(i1 %cmp10)
  call void @sink(i1 %cmp11)
  call void @sink(i1 %cmp12)

  br label %out

out:
  ret void
}

!0 = !{i1 0}
!1 = !{i1 1}
