; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare i64 @llvm.bswap.i64(i64) #0

define void @foo(i64 %x) {
entry:
  %cmp = icmp eq i64 %x, %x, !expected !1
  ret void
}

!1 = metadata !{ i1 1 }
