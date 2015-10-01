; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -souper-exploit-blockpcs -check %t

define i32 @foo(i32 %a) #0 {
entry:
  switch i32 %a, label %sw.default [
    i32 0, label %sw.bb
    i32 1, label %sw.bb1
    i32 2, label %sw.bb3
    i32 3, label %sw.bb5
    i32 4, label %sw.bb7
    i32 5, label %sw.bb9
    i32 6, label %sw.bb11
    i32 7, label %sw.bb13
    i32 8, label %sw.bb15
    i32 9, label %sw.bb17
    i32 10, label %sw.bb19
  ]

sw.bb:                                            ; preds = %entry
  %add = add nsw i32 10, %a
  br label %sw.epilog

sw.bb1:                                           ; preds = %entry
  %add2 = add nsw i32 9, %a
  br label %sw.epilog

sw.bb3:                                           ; preds = %entry
  %add4 = add nsw i32 8, %a
  br label %sw.epilog

sw.bb5:                                           ; preds = %entry
  %add6 = add nsw i32 7, %a
  br label %sw.epilog

sw.bb7:                                           ; preds = %entry
  %add8 = add nsw i32 6, %a
  br label %sw.epilog

sw.bb9:                                           ; preds = %entry
  %add10 = add nsw i32 5, %a
  br label %sw.epilog

sw.bb11:                                          ; preds = %entry
  %add12 = add nsw i32 4, %a
  br label %sw.epilog

sw.bb13:                                          ; preds = %entry
  %add14 = add nsw i32 3, %a
  br label %sw.epilog

sw.bb15:                                          ; preds = %entry
  %add16 = add nsw i32 2, %a
  br label %sw.epilog

sw.bb17:                                          ; preds = %entry
  %add18 = add nsw i32 1, %a
  br label %sw.epilog

sw.bb19:                                          ; preds = %entry
  %add20 = add nsw i32 0, %a
  br label %sw.epilog

sw.default:                                       ; preds = %entry
  br label %sw.epilog

sw.epilog:                                        ; preds = %sw.default, %sw.bb19, %sw.bb17, %sw.bb15, %sw.bb13, %sw.bb11, %sw.bb9, %sw.bb7, %sw.bb5, %sw.bb3, %sw.bb1, %sw.bb
  %c.0 = phi i32 [ 10, %sw.default ], [ %add20, %sw.bb19 ], [ %add18, %sw.bb17 ], [ %add16, %sw.bb15 ], [ %add14, %sw.bb13 ], [ %add12, %sw.bb11 ], [ %add10, %sw.bb9 ], [ %add8, %sw.bb7 ], [ %add6, %sw.bb5 ], [ %add4, %sw.bb3 ], [ %add2, %sw.bb1 ], [ %add, %sw.bb ]
  %cmp = icmp eq i32 %c.0, 10, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.6.0 (219188)"}
!1 = !{i1 1}
