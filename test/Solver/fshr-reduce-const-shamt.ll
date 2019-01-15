; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i64 @llvm.fshr.i64(i64, i64, i64) nounwind readnone

define i64 @foo(i64 %a, i64 %b) {
entry:
  %funnelled = call i64 @llvm.fshr.i64(i64 %a, i64 %b, i64 65)
  %betterfunnelled = call i64 @llvm.fshr.i64(i64 %a, i64 %b, i64 1)
  %cmp = icmp eq i64 %funnelled, %betterfunnelled, !expected !1
  %conv = zext i1 %cmp to i64
  ret i64 %conv
}

!1 = !{i1 1}
