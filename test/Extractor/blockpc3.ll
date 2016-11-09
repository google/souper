; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t
; RUN: FileCheck %s < %t

; CHECK-NOT: cand

@x3 = common global i32 0, align 4
@x1 = common global i32 0, align 4
@x0 = common global i32 0, align 4
@x2 = common global i32 0, align 4

; Function Attrs: norecurse nounwind uwtable
define i32 @foo() #0 {
entry:
  %0 = load i32, i32* @x3, align 4
  %tobool = icmp eq i32 %0, 0
  br i1 %tobool, label %entry.if.end_crit_edge, label %if.then

entry.if.end_crit_edge:                           ; preds = %entry
  %.pre = load i32, i32* @x0, align 4
  %.pre2 = load i32, i32* @x1, align 4
  br label %if.end

if.then:                                          ; preds = %entry
  store i32 %0, i32* @x1, align 4
  %.pr = load i32, i32* @x0, align 4
  %cmp1 = icmp slt i32 %.pr, 1
  br i1 %cmp1, label %for.inc.preheader, label %if.end

for.inc.preheader:                                ; preds = %if.then
  store i32 1, i32* @x0, align 4
  br label %if.end

if.end:                                           ; preds = %entry.if.end_crit_edge, %if.then, %for.inc.preheader
  %1 = phi i32 [ %.pre2, %entry.if.end_crit_edge ], [ %0, %if.then ], [ %0, %for.inc.preheader ]
  %2 = phi i32 [ %.pre, %entry.if.end_crit_edge ], [ %.pr, %if.then ], [ 1, %for.inc.preheader ]
  %or = or i32 %1, %2
  store i32 %or, i32* @x1, align 4
  store i32 %or, i32* @x2, align 4
  ret i32 %or
}

