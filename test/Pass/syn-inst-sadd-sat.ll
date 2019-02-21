; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=sadd.sat -S -o - | %FileCheck %s

define i8 @sadd_saturating(i8 %x, i8 %y) {
; CHECK: define i8 @sadd_saturating(i8 %x, i8 %y) {
; CHECK-NEXT: call i8 @llvm.sadd.sat.i8(i8 %x, i8 %y)

  %agg = call {i8, i1} @llvm.sadd.with.overflow.i8(i8 %x, i8 %y)
  %ov = extractvalue {i8, i1} %agg, 1
  %sum = extractvalue {i8, i1} %agg, 0
  %tmp = or i8 %x, %y
  %r_neg = icmp slt i8 %tmp, 0
  %clamped = select i1 %r_neg, i8 -128, i8 127
  %v1 = select i1 %ov, i8 %clamped, i8 %sum
  ret i8 %v1
}

declare { i8, i1 } @llvm.sadd.with.overflow.i8(i8, i8)
