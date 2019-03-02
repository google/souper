; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=and,const,or,add,xor -S -o - %s | %FileCheck %s

define i32 @ia32_Blsr_unsupported(i32) local_unnamed_addr #0 {
  %2 = sub i32 0, %0
  %3 = or i32 %2, %0
  %4 = add i32 %3, %0
  ret i32 %4

  ;CHECK-NOT: %2 = sub i32 0, %0
  ;CHECK-NOT: %3 = or i32 %2, %0
  ;CHECK-NOT: %4 = add i32 %3, %0
  ;CHECK: %2 = add i32 %0, -1
  ;CHECK: %3 = and i32 {{(%0, %2)|(%2, %0)}}

}
