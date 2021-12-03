; RUN: %opt -load-pass-plugin %pass -passes='function(souper)'  -S -stats -o - %s 2>&1 | %FileCheck %s

; The main thing being tested here is just that the verifier doesn't error
; because a PHI node comes after another instruction.

@c = local_unnamed_addr global i32 0, align 4
@d = local_unnamed_addr global i32 0, align 4
@e = local_unnamed_addr global i64 0, align 8
@b = local_unnamed_addr global i32 0, align 4
@a = global [1 x i32] zeroinitializer, align 4
@f = global [1 x i32] zeroinitializer, align 4

; CHECK: define void @g()
define void @g() {
entry:
  br i1 icmp ule (i32* getelementptr inbounds ([1 x i32], [1 x i32]* @a, i64 0, i64 0), i32* inttoptr (i64 6 to i32*)), label %for.body.lr.ph, label %for.end14

for.body.lr.ph:
  %b.promoted = load i32, i32* @b, align 4
  %e.promoted = load i64, i64* @e, align 8
  br label %for.inc10

for.inc10:
  %0 = sub nuw nsw i32 1, %b.promoted
  %cmp = icmp sge i32 %b.promoted, 0
  br i1 %cmp, label %for.inc10, label %for.inc13

for.inc13:
  %.lcssa = phi i32 [ %0, %for.inc10 ]
  %dec.6.lcssa = phi i64 [ %e.promoted, %for.inc10 ]
  %inc = add nsw i32 %b.promoted, 1
  br i1 icmp ule (i32* getelementptr inbounds ([1 x i32], [1 x i32]* @a, i64 0, i64 0), i32* inttoptr (i64 6 to i32*)), label %for.inc10, label %for.cond.for.end14_crit_edge

for.cond.for.end14_crit_edge:
  %.lcssa.lcssa = phi i32 [ %.lcssa, %for.inc13 ]
  %dec.6.lcssa.lcssa = phi i64 [ %dec.6.lcssa, %for.inc13 ]
  %inc.lcssa = phi i32 [ %inc, %for.inc13 ]
  store i64 %dec.6.lcssa.lcssa, i64* @e, align 8
  br label %for.end14

for.end14:
  ret void
}

; CHECK: - Number of insts removed

