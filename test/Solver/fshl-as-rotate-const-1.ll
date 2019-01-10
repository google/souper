; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i48 @llvm.fshl.i48(i48, i48, i48) nounwind readnone

define i48 @foo() {
entry:                            ; 0x000000123456 0x000000123456
  %funnelled = call i48 @llvm.fshl.i48(i48 1193046,   i48 1193046, i48 32)
  ;                              0x345600000012
  %cmp = icmp eq i48 %funnelled, 57543971831826, !expected !1
  %conv = zext i1 %cmp to i48
  ret i48 %conv
}

!1 = !{i1 1}
