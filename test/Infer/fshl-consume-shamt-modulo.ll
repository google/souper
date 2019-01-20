; REQUIRES: solver

; RUN: %llvm-as %s -o - | %souper %solver -souper-infer-inst -souper-synthesis-comps=fshl | %FileCheck %s

declare i64 @llvm.fshl.i64(i64, i64, i64) nounwind readnone

define i64 @foo(i64 %a, i64 %b, i64 %n) {
; CHECK: ; Function: foo
; CHECK-NEXT: %0:i64 = var
; CHECK-NEXT: %1:i64 = var
; CHECK-NEXT: %2:i64 = var
; CHECK-NEXT: %3:i64 = urem %2, 64:i64
; CHECK-NEXT: %4:i64 = fshl %0, %1, %3
; CHECK-NEXT: %5:i64 = fshl %0, %1, %2
; CHECK-NEXT: cand %4 %5

  %nmodwidth = urem i64 %n, 64
  %funnelled = call i64 @llvm.fshl.i64(i64 %a, i64 %b, i64 %nmodwidth)
  ret i64 %funnelled
}
