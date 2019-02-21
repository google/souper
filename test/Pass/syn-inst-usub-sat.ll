; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=usub.sat -S -o - | %FileCheck %s

define i8 @usub_saturating(i8 %x, i8 %y) {
; CHECK: define i8 @usub_saturating(i8 %x, i8 %y) {
; CHECK-NEXT: call i8 @llvm.usub.sat.i8(i8 %x, i8 %y)

  %agg = call {i8, i1} @llvm.usub.with.overflow.i8(i8 %x, i8 %y)
  %ov = extractvalue {i8, i1} %agg, 1
  %sum = extractvalue {i8, i1} %agg, 0
  %v1 = select i1 %ov, i8 0, i8 %sum
  ret i8 %v1
}

declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8)
