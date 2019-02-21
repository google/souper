; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind readnone
declare i19 @llvm.bitreverse.i19(i19) #0

define i1 @foo(i19 %x) {
entry:
  %rev = call i19 @llvm.bitreverse.i19(i19 238764)
  %cmp = icmp eq i19 %rev, 108846, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
