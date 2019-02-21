; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind readnone
declare i32 @llvm.bitreverse.i32(i32) #0

define i1 @foo(i32 %x) {
entry:
  %rev = call i32 @llvm.bitreverse.i32(i32 2882343476)
  %cmp = icmp eq i32 %rev, 742962133, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
