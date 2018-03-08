; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=add,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = add i32 %x, 1
  %a = add i32 %x, 1
  ;CHECK-NOT: %b = add i32 %a, 1
  %b = add i32 %a, 1
  ;CHECK-NOT: %c = add i32 %b, 1
  %c = add i32 %b, 1
  ;CHECK: add i32 %x, 4
  %d = add i32 %c, 1
  ret i32 %d
}
