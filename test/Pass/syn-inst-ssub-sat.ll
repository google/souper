; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=ssub.sat -S -o - | %FileCheck %s

define i8 @ssub_saturating(i8 %x, i8 %y) {
; CHECK: define i8 @ssub_saturating(i8 %x, i8 %y) {
; CHECK-NEXT: call i8 @llvm.ssub.sat.i8(i8 %x, i8 %y)

  %agg = call {i8, i1} @llvm.ssub.with.overflow.i8(i8 %x, i8 %y)
  %ov = extractvalue {i8, i1} %agg, 1
  %sum = extractvalue {i8, i1} %agg, 0
  %x_lt0 = icmp slt i8 %x, 0
  %y_gt0 = icmp sgt i8 %y, 0
  %t = and i1 %x_lt0, %y_gt0
  %clamped = select i1 %t, i8 -128, i8 127
  %v1 = select i1 %ov, i8 %clamped, i8 %sum
  ret i8 %v1
}

declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8)
