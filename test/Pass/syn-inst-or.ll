; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=or,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = or i32 %x, 3
  %a = or i32 %x, 3 ; 0b011
  ;CHECK-NOT: %b = or i32 %a, 6
  %b = or i32 %a, 6 ;0b110
  ;CHECK: or i32 %x, 7
  ret i32 %b
}
