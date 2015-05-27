
; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

%class.A = type { i8 }

@a = global i32 0, align 4

; Function Attrs: nounwind uwtable
define noalias i32* @foo(%class.A* nocapture readnone %this) #0 align 2 {
entry:
  %0 = load i32, i32* @a, align 4, !tbaa !1
  %tobool = icmp eq i32 %0, 0
  br i1 %tobool, label %for.body, label %land.rhs

land.rhs:                                         ; preds = %entry
  %call = tail call i32 @bar() #2
  br label %for.bodythread-pre-split

for.bodythread-pre-split:                         ; preds = %for.inc, %land.rhs
  %K.010.ph = phi i32 [ undef, %land.rhs ], [ %inc, %for.inc ]
  %.pr = load i32, i32* @a, align 4, !tbaa !1
  %phitmp = icmp eq i32 %.pr, 0
  br label %for.body

for.body:                                         ; preds = %for.bodythread-pre-split, %entry
  %1 = phi i1 [ %phitmp, %for.bodythread-pre-split ], [ true, %entry ]
  %K.010 = phi i32 [ %K.010.ph, %for.bodythread-pre-split ], [ undef, %entry ]
  br i1 %1, label %for.inc, label %land.rhs5

land.rhs5:                                        ; preds = %for.body
  %call6 = tail call i32 @bar() #2
  br label %for.inc

for.inc:                                          ; preds = %land.rhs5, %for.body
  %inc = add nsw i32 %K.010, 1
  %tobool3 = icmp eq i32 %inc, 0
  br i1 %tobool3, label %for.end, label %for.bodythread-pre-split

for.end:                                          ; preds = %for.inc
  ret i32* null
}

declare i32 @bar() #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.5.0 (213048)"}
!1 = !{!2, !2, i64 0}
!2 = !{!"int", !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
