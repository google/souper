; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i8 @llvm.fshr.i8(i8, i8, i8) nounwind readnone

define i8 @foo() {
entry:                               ; 0xAA   0x55
  %funnelled = call i8 @llvm.fshr.i8(i8 170, i8 85, i8 4)
  ;                            0xA5
  %cmp = icmp eq i8 %funnelled, 165, !expected !1
  %conv = zext i1 %cmp to i8
  ret i8 %conv
}

!1 = !{i1 1}
