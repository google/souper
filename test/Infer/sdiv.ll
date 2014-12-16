; REQUIRES: solver

; RUN: opt -load %pass -souper %solver %s -souper-infer-nop -adce | llvm-dis -o - | FileCheck %s
; CHECK-NOT: sdiv
; CHECK-NOT: shl

define i64 @f(i1 %in0, i64 %in1) #0 {
entry:
  %0 = and i64 %in1, 1
  %1 = icmp eq i64 %0, 0
  %2 = and i1 %in0, %1
  br i1 %2, label %cont, label %out
cont:
  %3 = add i64 -2, %in1
  %4 = sdiv i64 %3, 2               ; killed by nop inference
  %5 = shl i64 %4, 1                ; killed by nop inference
  ret i64 %5
out:
  ret i64 0
}
