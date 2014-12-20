; REQUIRES: solver

; RUN: opt -load %pass -souper %solver %s -souper-infer-nop -adce -S -o - | FileCheck %s
; CHECK-NOT: add i32
; CHECK-NOT: or i32
; CHECK-NOT: sub i32

define i32 @f(i32 %a) #0 {
entry:
  %0 = add i32 %a, 0
  %1 = add i32 %0, 0
  %2 = add i32 %1, 0
  %3 = add i32 %2, 0
  %4 = add i32 %3, 0
  %5 = add i32 %4, 0
  %6 = add i32 %5, 0
  %7 = add i32 %6, 0
  %8 = shl i32 %7, 1
  %9 = or i32 1, %8
  %10 = sub i32 %9, 1
  ret i32 %10
}
