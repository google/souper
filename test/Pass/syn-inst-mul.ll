; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=mul,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = mul i32 %x, 1
  %a = mul i32 %x, 1
  ;CHECK-NOT: %b = mul i32 %a, 2
  %b = mul i32 %a, 2
  ;CHECK-NOT: %c = mul i32 %b, 3
  %c = mul i32 %b, 3
  ;CHECK-NOT: %d = mul i32 %c, 4
  %d = mul i32 %c, 4
  ;CHECK: mul i32 %x, 24
  ret i32 %d
}
