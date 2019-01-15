; REQUIRES: solver

; RUN: %llvm-as %s -o - | %souper %solver -souper-infer-nop | %FileCheck %s

declare i64 @llvm.fshr.i64(i64, i64, i64) nounwind readnone

define i64 @foo(i64 %a, i64 %b) {
; CHECK: ; Function: foo
; CHECK-NEXT: %0:i64 = var
; CHECK-NEXT: %1:i64 = var
; CHECK-NEXT: %2:i64 = fshr %0, %1, 0:i64
; CHECK-NEXT: cand %2 %1

  %funnelled = call i64 @llvm.fshr.i64(i64 %a, i64 %b, i64 0)
  ret i64 %funnelled
}
