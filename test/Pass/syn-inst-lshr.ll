; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=lshr,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = lshr i32 %x, 5
  %a = lshr i32 %x, 5
  ;CHECK-NOT: %b = lshr i32 %a, 8
  %b = lshr i32 %a, 8
  ;CHECK: lshr i32 %x, 13
  ret i32 %b
}
