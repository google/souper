; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=and -S -o - %s | %FileCheck %s

define i32 @alive0(i32, i32) local_unnamed_addr #0 {
  %Op0 = or i32 %0, %1
  %Op1 = xor i32 %0, %1
  %r = sub i32 %Op0, %Op1
  ret i32 %r

  ;CHECK-NOT: %Op0 = or i32 %0, %1
  ;CHECK-NOT: %Op1 = xor i32 %0, %1
  ;CHECK-NOT: %r = sub i32 %Op0, %Op1
  ;CHECK: %3 = and i32 %0, %1

}
