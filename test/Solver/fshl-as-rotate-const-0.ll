; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i8 @llvm.fshl.i8(i8, i8, i8) nounwind readnone

define i8 @foo() {
entry:                               ; 0xAB    0xAB
  %funnelled = call i8 @llvm.fshl.i8(i8 171, i8 171, i8 4)
  ;                            0xBA
  %cmp = icmp eq i8 %funnelled, 186, !expected !1
  %conv = zext i1 %cmp to i8
  ret i8 %conv
}

!1 = !{i1 1}
