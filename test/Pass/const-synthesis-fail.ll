; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=and,or,ne,xor,const,add,sub,ashr -S -o - %s | %FileCheck %s
;XFAIL: *

define i64 @alive0_f0(i64) local_unnamed_addr #0 {
  %c = icmp slt i64 -1, %0
  %r = select i1 %c, i64 -8494242232709579858, i64 -3671342357661587618
  ret i64 %r

  ;CHECK-NOT: %c = icmp slt i64 -1, %0
  ;CHECK-NOT: %r = select i1 %c, i64 -8494242232709579858, i64 -3671342357661587618
  ;CHECK: %3 = ashr i64 %0, 63
  ;CHECK: %4 = and i64 %3, 4822899875047992240
  ;CHECK: %5 = add i64 %4, -8494242232709579858
}

define i32 @alive0_f1(i32) local_unnamed_addr #0 {
  %c = icmp slt i32 %0, 0
  %r = select i1 %c, i32 -1565908448, i32 -1696780520
  ret i32 %r

  ;CHECK-NOT: %c = icmp slt i32 %0, 0
  ;CHECK-NOT: %r = select i1 %c, i32 -1565908448, i32 -1696780520
  ;CHECK: %3 = ashr i32 %0, 31
  ;CHECK: %4 = and i32 %3, 130872072
  ;CHECK: %5 = add i32 %4, -1696780520
}
