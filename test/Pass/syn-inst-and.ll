; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=and,const -S -o - %s | %FileCheck %s


define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = and i32 %x, 3
  %a = and i32 %x, 3 ; 0b011
  ;CHECK-NOT: %b = and i32 %a, 6
  %b = and i32 %a, 6 ;0b110
  ;CHECK: and i32 %x, 2
  ret i32 %b
}
