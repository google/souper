; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-iN=false -souper-infer-inst -souper-synthesis-comps=select,const,add -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x, i1 %y) {
  ;CHECK-NOT: %1 = xor i32 %x, %x
  %1 = xor i32 %x, %x
  ;CHECK-NOT: %2 = add i32 %1, 17
  %2 = add i32 %1, 17
  ;CHECK-NOT: %3 = add i32 %1, 21
  %3 = add i32 %1, 21
  ;CHECK-NOT: %4 = select i1 %y, i32 %2, i32 %3
  %4 = select i1 %y, i32 %2, i32 %3
  ;CHECK: select i1 %y, i32 17, i32 21
  ret i32 %4
}
