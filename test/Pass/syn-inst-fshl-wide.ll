

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce -souper-infer-inst -souper-synthesis-comps=fshl -S -o - | %FileCheck %s

define i32 @naive(i32 %a, i32 %b, i32 %c) {
; CHECK: define i32 @naive(i32 %a, i32 %b, i32 %c) {
; CHECK-NEXT: %1 = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
; CHECK-NEXT: ret i32 %1
; CHECK-NEXT: }

  %widea = zext i32 %a to i64
  %wideb = zext i32 %b to i64
  %high = shl i64 %widea, 32
  %wide = or i64 %high, %wideb
  %cmodwidth = and i32 %c, 31
  %cmodwidthwide = zext i32 %cmodwidth to i64
  %shifted = shl i64 %wide, %cmodwidthwide
  %skiplowhalf = lshr i64 %shifted, 32
  %trunc = trunc i64 %skiplowhalf to i32
  ret i32 %trunc
}
