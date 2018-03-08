; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=sub,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = sub i32 %x, 1
  %a = sub i32 %x, 1
  ;CHECK-NOT: %b = sub i32 %a, 1
  %b = sub i32 %a, 1
  ;CHECK-NOT: %c = sub i32 %b, 1
  %c = sub i32 %b, 1
  ;CHECK-NOT: %d = sub i32 %c, 1
  %d = sub i32 %c, 1
  ;CHECK: sub i32 %x, 4
  ret i32 %d
}
