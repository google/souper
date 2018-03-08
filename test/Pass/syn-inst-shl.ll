; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=shl,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = add i32 %x, %x
  %a = add i32 %x, %x
  ;CHECK-NOT: %b = add i32 %a, %a
  %b = add i32 %a, %a
  ;CHECK: shl i32 %x, 2
  ret i32 %b
}
