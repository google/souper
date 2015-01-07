; REQUIRES: solver

; RUN: opt -load %pass -souper %solver %s -souper-infer-nop -adce -S -o - | FileCheck %s
; CHECK-NOT: -8

define i64 @f(i64 %a) #0 {
entry:
  %0 = and i64 %a, 15
  %1 = icmp ne i64 %0, 0
  br i1 %1, label %out, label %cont
cont:
  %2 = and i64 %a, -8                     ; killed by nop inference
  ret i64 %2
out:
  ret i64 0
}
