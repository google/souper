; REQUIRES: solver

; RUN: %llvm-as %s -o - | %souper %solver -souper-infer-inst -souper-synthesis-comps=fshl,const | %FileCheck %s

; XFAIL: *
; FIXME: does not seem to try to reduce the constant.

declare i64 @llvm.fshl.i64(i64, i64, i64) nounwind readnone

define i64 @foo(i64 %a, i64 %b) {
; CHECK: ; Function: foo
; CHECK-NEXT: %0:i64 = var
; CHECK-NEXT: %1:i64 = var
; CHECK-NEXT: %2:i64 = fshl %0, %1, 65:i64
; CHECK-NEXT: %3:i64 = fshl %0, %1, 1:i64
; CHECK-NEXT: cand %2 %3

  %funnelled = call i64 @llvm.fshl.i64(i64 %a, i64 %b, i64 65)
  ret i64 %funnelled
}
