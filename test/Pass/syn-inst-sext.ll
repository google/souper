; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -S -o - %s | %FileCheck %s

define i8 @foo(i1 %x) {
entry:
  ;CHECK-NOT: %a = select i1 %x, i1 0, i1 1
  %a = select i1 %x, i1 0, i1 1
  ;CHECK-NOT: %b = select i1 %a, i8 0, i8 -1
  %b = select i1 %a, i8 0, i8 -1
  ;CHECK: sext i1 %x to i8
  ret i8 %b
}
