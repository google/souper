; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=add,sub,mul,const -S -o - %s | %FileCheck %s

; Translated from test/Infer/odd.opt

define i1 @foo(i8 %x) {
entry:
  ;CHECK-NOT: %a = urem i8 %x, 2
  %a = urem i8 %x, 2
  ;CHECK-NOT: %cmp = icmp eq i8 %a, 1
  %cmp = icmp eq i8 %a, 1
  ;CHECK: trunc i8 %x to i1
  ret i1 %cmp
}
