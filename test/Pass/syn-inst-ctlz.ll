; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=ctlz -S -o - %s | %FileCheck %s

define i8 @func(i8 %v) local_unnamed_addr #0 {
entry:
  ;CHECK-NOT: %conv = zext i8 %v to i64
  %conv = zext i8 %v to i64
  ;CHECK-NOT: %mul = mul nuw nsw i64 %conv, 8623620610
  %mul = mul nuw nsw i64 %conv, 8623620610
  ;CHECK-NOT: %and = and i64 %mul, 1136090292240
  %and = and i64 %mul, 1136090292240
  ;CHECK-NOT: %rem = urem i64 %and, 1023
  %rem = urem i64 %and, 1023
  ;CHECK-NOT: %0 = trunc i64 %rem to i32
  %0 = trunc i64 %rem to i32
  ;CHECK-NOT: %sub = sub nsw i32 0, %0
  %sub = sub nsw i32 0, %0
  ;CHECK-NOT: %and4 = and i32 %0, %sub
  %and4 = and i32 %0, %sub
  ;CHECK-NOT: %conv5 = trunc i32 %and4 to i8
  %conv5 = trunc i32 %and4 to i8
  ;CHECK-NOT: %tobool = icmp eq i8 %conv5, 0
  %tobool = icmp eq i8 %conv5, 0
  ;CHECK-NOT: %. = select i1 %tobool, i8 8, i8 7
  %. = select i1 %tobool, i8 8, i8 7
  ;CHECK-NOT: %. = select i1 %tobool, i8 8, i8 7
  %and7 = and i32 %and4, 15
  ;CHECK-NOT: %tobool8 = icmp eq i32 %and7, 0
  %tobool8 = icmp eq i32 %and7, 0
  ;CHECK-NOT: %sub11 = add nsw i8 %., -4
  %sub11 = add nsw i8 %., -4
  ;CHECK-NOT: %c.1 = select i1 %tobool8, i8 %., i8 %sub11
  %c.1 = select i1 %tobool8, i8 %., i8 %sub11
  ;CHECK-NOT: %and15 = and i32 %and4, 51
  %and15 = and i32 %and4, 51;
  ;CHECK-NOT: %tobool16 = icmp eq i32 %and15, 0
  %tobool16 = icmp eq i32 %and15, 0
  ;CHECK-NOT: %sub19 = add nsw i8 %c.1, -2
  %sub19 = add nsw i8 %c.1, -2
  ;CHECK-NOT: %c.1.sub19 = select i1 %tobool16, i8 %c.1, i8 %sub19
  %c.1.sub19 = select i1 %tobool16, i8 %c.1, i8 %sub19
  ;CHECK-NOT: %and23 = and i32 %and4, 85
  %and23 = and i32 %and4, 85
  ;CHECK-NOT: %tobool24 = icmp ne i32 %and23, 0
  %tobool24 = icmp ne i32 %and23, 0
  ;CHECK-NOT: %sub27 = sext i1 %tobool24 to i8
  %sub27 = sext i1 %tobool24 to i8
  ;CHECK-NOT: %c.3 = add nsw i8 %c.1.sub19, %sub27
  %c.3 = add nsw i8 %c.1.sub19, %sub27
  ;CHECK: call i8 @llvm.ctlz.i8(i8 %v, i1 false)
  ret i8 %c.3
}


