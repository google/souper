; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper %solver -souper-infer-nop -souper-stress-nop -S -stats -o - %s 2>&1 | %FileCheck %s

; CHECK: store i32 %cond.i

@a = common global i32 0, align 4
@c = common global i32 0, align 4
@b = common global i32 0, align 4

define void @f() local_unnamed_addr #0 {
entry:
  %0 = load i32, i32* @a, align 4
  %tobool.i = icmp eq i32 %0, 0
  %cond.i = select i1 %tobool.i, i32 0, i32 -1
  store i32 %cond.i, i32* @c, align 4
  %shl.i = shl nsw i32 %cond.i, 2
  %or.i = or i32 %cond.i, %shl.i
  %shl1.i = shl nsw i32 %cond.i, 8
  %or2.i = or i32 %or.i, %shl1.i
  store i32 %or2.i, i32* @b, align 4
  ret void
}
