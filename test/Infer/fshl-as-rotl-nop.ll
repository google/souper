; REQUIRES: solver

; RUN: %llvm-as %s -o - | %souper %solver -souper-infer-nop | %FileCheck %s

declare i64 @llvm.fshl.i64(i64, i64, i64) nounwind readnone
declare i64 @llvm.fshr.i64(i64, i64, i64) nounwind readnone

define i64 @foo(i64 %a, i64 %n) {
; CHECK: ; Function: foo
; CHECK-NEXT: %0:i64 = var
; CHECK-NEXT: %1:i64 = var
; CHECK-NEXT: %2:i64 = fshl %0, %0, %1
; CHECK-NEXT: %3:i64 = fshr %2, %2, %1
; CHECK-NEXT: cand %3 %0

  %funnelled = call i64 @llvm.fshl.i64(i64 %a, i64 %a, i64 %n)
  %refunnelled = call i64 @llvm.fshr.i64(i64 %funnelled, i64 %funnelled, i64 %n)
  ret i64 %refunnelled
}
