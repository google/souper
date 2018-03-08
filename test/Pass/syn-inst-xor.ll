; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=xor,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = xor i32 %x, 5
  %a = xor i32 %x, 3 ; 0b011
  ;CHECK-NOT: %b = xor i32 %a, 6
  %b = xor i32 %a, 6 ;0b110
  ;CHECK: xor i32 %x, 5
  ret i32 %b
}
