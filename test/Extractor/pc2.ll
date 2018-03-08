
; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind
define i32 @fn1() #0 {
entry:
  %0 = add i32 4, 0
  %tobool = icmp eq i32 %0, 0, !expected !0
  br i1 %tobool, label %if.end13, label %for.cond.preheader

for.cond.preheader:                               ; preds = %entry
  %1 = add i32 1, 0
  %tobool119 = icmp eq i32 1, 0, !expected !0
  br i1 %tobool119, label %if.end.lr.ph, label %for.end.loopexit

if.end.lr.ph:                                     ; preds = %for.cond.preheader
  br label %if.end

if.end:                                           ; preds = %if.end.lr.ph, %cleanup
  %d.020 = phi i64 [ undef, %if.end.lr.ph ], [ %d.1, %cleanup ]
  %tobool3 = icmp eq i64 %d.020, 0
  %d.1 = select i1 %tobool3, i64 0, i64 1
  %tobool6 = icmp eq i1 %tobool3, 0
  br i1 %tobool6, label %cleanup, label %if.then7

if.then7:                                         ; preds = %if.end
  br label %for.end

cleanup:                                          ; preds = %if.end
  %tobool1 = icmp eq i32 1, 0, !expected !0
  br i1 %tobool1, label %if.end, label %for.cond.for.end.loopexit_crit_edge

for.cond.for.end.loopexit_crit_edge:              ; preds = %cleanup
  br label %for.end.loopexit

for.end.loopexit:                                 ; preds = %for.cond.for.end.loopexit_crit_edge, %for.cond.preheader
  %d.0.lcssa = phi i64 [ %d.1, %for.cond.for.end.loopexit_crit_edge ], [ undef, %for.cond.preheader ]
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %if.then7
  %d.2.ph = phi i64 [ %d.1, %if.then7 ], [ %d.0.lcssa, %for.end.loopexit ]
  %tobool10 = icmp eq i64 %d.2.ph, 0
  br label %if.end13

if.end13:                                         ; preds = %for.end, %entry, %if.then11
  ret i32 undef
}

!0 = !{i1 0}
