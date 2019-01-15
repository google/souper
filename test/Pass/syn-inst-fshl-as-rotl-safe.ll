; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=fshl -S -o - | %FileCheck %s

define i32 @rotate(i32 %x, i32 %n) {
; CHECK: define i32 @rotate(i32 %x, i32 %n) {
; CHECK-NEXT: %1 = call i32 @llvm.fshl.i32(i32 %x, i32 %x, i32 %n)
; CHECK-NEXT: ret i32 %1
; CHECK-NEXT: }

  %nmodwidth = and i32 %n, 31
  %negnmodwidth = sub i32 32, %nmodwidth
  %highpart = shl i32 %x, %nmodwidth
  %lowpart = lshr i32 %x, %negnmodwidth
  %conv2 = or i32 %highpart, %lowpart
  ret i32 %conv2
}
