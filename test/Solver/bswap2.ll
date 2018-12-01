; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare i32 @llvm.bswap.i32(i32) #0
declare i32 @llvm.ctpop.i32(i32) #1

define i1 @foo(i32 %x) {
entry:
  %count1 = call i32 @llvm.ctpop.i32(i32 %x)
  %swap = call i32 @llvm.bswap.i32(i32 %x)
  %count2 = call i32 @llvm.ctpop.i32(i32 %swap)
  %cmp = icmp eq i32 %count1, %count2, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
