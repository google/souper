; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare i64 @llvm.bswap.i64(i64) #0

define i1 @foo(i64 %x) {
entry:
  %swap1 = call i64 @llvm.bswap.i64(i64 %x)
  %swap2 = call i64 @llvm.bswap.i64(i64 %swap1)
  %cmp = icmp eq i64 %x, %swap2, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
