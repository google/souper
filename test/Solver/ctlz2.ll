; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i32 @llvm.ctlz.i32(i32) nounwind readnone

define i32 @foo(i32 %x) {
entry:
  %count = call i32 @llvm.ctlz.i32(i32 %x)
  %shr = lshr i32 %x, 4
  %count2 = call i32 @llvm.ctlz.i32(i32 %shr)
  %diff = sub nuw i32 %count2, %count
  %cmp = icmp ule i32 %diff, 4, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = !{i1 1}
