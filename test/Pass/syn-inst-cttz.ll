; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=cttz -S -o - %s | %FileCheck %s

define i32 @func(i32 %v) local_unnamed_addr #0 {
entry:
  ;CHECK-NOT: %sub = sub nsw i32 0, %v
  %sub = sub nsw i32 0, %v
  ;CHECK-NOT: %and = and i32 %sub, %v
  %and = and i32 %sub, %v
  ;CHECK-NOT: %tobool = icmp eq i32 %and, 0
  %tobool = icmp eq i32 %and, 0
  ;CHECK-NOT: %. = select i1 %tobool, i32 32, i32 31
  %. = select i1 %tobool, i32 32, i32 31
  ;CHECK-NOT: %and1 = and i32 %and, 65535
  %and1 = and i32 %and, 65535
  ;CHECK-NOT: %tobool2 = icmp eq i32 %and1, 0
  %tobool2 = icmp eq i32 %and1, 0
  ;CHECK-NOT: %sub4 = add nsw i32 %., -16
  %sub4 = add nsw i32 %., -16
  ;CHECK-NOT: %c.1 = select i1 %tobool2, i32 %., i32 %sub4
  %c.1 = select i1 %tobool2, i32 %., i32 %sub4
  ;CHECK-NOT: %and6 = and i32 %and, 16711935
  %and6 = and i32 %and, 16711935
  ;CHECK-NOT: %tobool7 = icmp eq i32 %and6, 0
  %tobool7 = icmp eq i32 %and6, 0
  ;CHECK-NOT: %sub9 = add nsw i32 %c.1, -8
  %sub9 = add nsw i32 %c.1, -8
  ;CHECK-NOT: %c.1.sub9 = select i1 %tobool7, i32 %c.1, i32 %sub9
  %c.1.sub9 = select i1 %tobool7, i32 %c.1, i32 %sub9
  ;CHECK-NOT: %and11 = and i32 %and, 252645135
  %and11 = and i32 %and, 252645135
  ;CHECK-NOT: %tobool12 = icmp eq i32 %and11, 0
  %tobool12 = icmp eq i32 %and11, 0
  ;CHECK-NOT: %sub14 = add nsw i32 %c.1.sub9, -4
  %sub14 = add nsw i32 %c.1.sub9, -4
  ;CHECK-NOT: %c.3 = select i1 %tobool12, i32 %c.1.sub9, i32 %sub14
  %c.3 = select i1 %tobool12, i32 %c.1.sub9, i32 %sub14
  ;CHECK-NOT: %and16 = and i32 %and, 858993459
  %and16 = and i32 %and, 858993459
  ;CHECK-NOT: %tobool17 = icmp eq i32 %and16, 0
  %tobool17 = icmp eq i32 %and16, 0
  ;CHECK-NOT: %sub19 = add i32 %c.3, -2
  %sub19 = add i32 %c.3, -2
  ;CHECK-NOT: %c.3.sub19 = select i1 %tobool17, i32 %c.3, i32 %sub19
  %c.3.sub19 = select i1 %tobool17, i32 %c.3, i32 %sub19
  ;CHECK-NOT: %and21 = and i32 %and, 1431655765
  %and21 = and i32 %and, 1431655765
  ;CHECK-NOT: %tobool22 = icmp ne i32 %and21, 0
  %tobool22 = icmp ne i32 %and21, 0
  ;CHECK-NOT: %sub24 = sext i1 %tobool22 to i32
  %sub24 = sext i1 %tobool22 to i32
  ;CHECK-NOT: %c.5 = add i32 %c.3.sub19, %sub24
  %c.5 = add i32 %c.3.sub19, %sub24
  ;CHECK: %0 = call i32 @llvm.cttz.i32(i32 %v, i1 false)
  ret i32 %c.5
}


