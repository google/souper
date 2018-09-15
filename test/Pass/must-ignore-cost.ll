; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-ignore-cost -souper-synthesis-comps=and,or,ne,xor,const,add,sub,ashr -S -o - %s | %FileCheck %s

define i1 @alive0_f2(i16, i16) local_unnamed_addr #0 {
  %cmp1 = icmp ne i16 %0, 0
  %cmp2 = icmp ne i16 %1, 0
  %r = or i1 %cmp1, %cmp2
  ret i1 %r

  ;CHECK-NOT: %cmp1 = icmp ne i16 %0, 0
  ;CHECK-NOT: %cmp2 = icmp ne i16 %1, 0
  ;CHECK-NOT: %r = or i1 %cmp1, %cmp2
  ;CHECK: %3 = or i16 %0, %1
  ;CHECK: %4 = icmp ne i16 %3, 0
}
