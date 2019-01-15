; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare i32 @llvm.cttz.i32(i32) nounwind readnone

define i1 @foo(i32 %x, i32 %y) {
entry:
  %zero = icmp eq i32 %x, 0
  %count1 = call i32 @llvm.cttz.i32(i32 %x)
  %shift = shl nsw nuw i32 %x, %y
  %count2 = call i32 @llvm.cttz.i32(i32 %shift)
  %count1plusy = add i32 %count1, %y
  %eq = icmp eq i32 %count1plusy, %count2
  %cmp = or i1 %eq, %zero, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
