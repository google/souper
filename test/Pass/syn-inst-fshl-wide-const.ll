; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=fshl,const -S -o - | %FileCheck %s

define i32 @naive(i32 %a, i32 %b) {
; CHECK: define i32 @naive(i32 %a, i32 %b) {
; CHECK-NEXT: %1 = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 16)
; CHECK-NEXT: ret i32 %1
; CHECK-NEXT: }

  %widea = zext i32 %a to i64
  %wideb = zext i32 %b to i64
  %high = shl i64 %widea, 32
  %wide = or i64 %high, %wideb
  %skiplowhalf = lshr i64 %wide, 16
  %trunc = trunc i64 %skiplowhalf to i32
  ret i32 %trunc
}
