; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=fshr -S -o - | %FileCheck %s

; FIXME: do we really want to synthesize fshl here?

define i32 @rotate(i32 %x, i32 %n) {
; CHECK: define i32 @rotate(i32 %x, i32 %n) {
; CHECK-NEXT: %1 = call i32 @llvm.fshr.i32(i32 %x, i32 %x, i32 %n)
; CHECK-NEXT: ret i32 %1
; CHECK-NEXT: }

  %negnmodwidth = sub i32 32, %n
  %highpart = shl i32 %x, %negnmodwidth
  %lowpart = lshr i32 %x, %n
  %conv2 = or i32 %highpart, %lowpart
  ret i32 %conv2
}
