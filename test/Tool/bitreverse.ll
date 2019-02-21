; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare i67 @llvm.bitreverse.i67(i67) #0

define i1 @foo(i67 %x) {
entry:
  %rev1 = call i67 @llvm.bitreverse.i67(i67 %x)
  %rev2 = call i67 @llvm.bitreverse.i67(i67 %rev1)
  %cmp = icmp eq i67 %x, %rev2, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
