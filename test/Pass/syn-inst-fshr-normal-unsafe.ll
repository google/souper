; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=fshr -S -o - | %FileCheck %s

; FIXME: do we really want to synthesize fshl here?

define i32 @normal(i32 %a, i32 %b, i32 %c) {
; CHECK: define i32 @normal(i32 %a, i32 %b, i32 %c) {
; CHECK-NEXT: %1 = call i32 @llvm.fshr.i32(i32 %a, i32 %b, i32 %c)
; CHECK-NEXT: ret i32 %1
; CHECK-NEXT: }

  %negc = sub i32 32, %c
  %highpart = shl i32 %a, %negc
  %lowpart = lshr i32 %b, %c
  %conv2 = or i32 %highpart, %lowpart
  ret i32 %conv2
}
