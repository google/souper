; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i32 @llvm.fshl.i32(i32, i32, i32) nounwind readnone

define i32 @foo() {
entry:                                 ; 0x00ABCDEF     0x12345678
  %funnelled = call i32 @llvm.fshl.i32(i32 11259375, i32 305419896, i32 12)
  ;                              0xBCDEF123
  %cmp = icmp eq i32 %funnelled, 3168727331, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = !{i1 1}
