; REQUIRES: solver

; RUN: %llvm-as %s -o - | %souper %solver -souper-infer-inst -souper-synthesis-comps=and,lshr,const | %FileCheck %s

declare i64 @llvm.fshr.i64(i64, i64, i64) nounwind readnone

define i64 @foo(i64 %b, i64 %c) {
; CHECK: ; Function: foo
; CHECK-NEXT: %0:i64 = var
; CHECK-NEXT: %1:i64 = var
; CHECK-NEXT: %2:i64 = fshr 0:i64, %0, %1
; CHECK-NEXT: %3:i64 = and 63:i64, %1
; CHECK-NEXT: %4:i64 = lshr %0, %3
; CHECK-NEXT: cand %2 %4

  %funnelled = call i64 @llvm.fshr.i64(i64 0, i64 %b, i64 %c)
  ret i64 %funnelled
}
