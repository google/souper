; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define zeroext i1 @foo(i32 %a, i32 %b) {
entry:
  switch i32 %a, label %default [
    i32 0, label %bb4
    i32 1, label %bb4
    i32 2, label %bb4
  ]

default:                                    ; preds = %entry
  %cmp1 = icmp ne i32 %a, 0, !expected !1
  br i1 %cmp1, label %bb1, label %bb5

bb1:                                        ; preds = %default
  %cmp2 = icmp ne i32 %a, 1, !expected !1
  br i1 %cmp2, label %bb2, label %bb5

bb2:                                        ; preds = %bb1
  %cmp3 = icmp ne i32 %a, 2, !expected !1
  br i1 %cmp3, label %bb3, label %bb5

bb3:                                        ; preds = %bb2
  br label %bb5

bb4:                                        ; preds = %entry
  br label %bb5

bb5:                                        ; preds = %bb1, %bb2, %bb3, %bb4, %default
  %phi1 = phi i32 [%b, %bb4], [ %b, %bb3 ], [ %b, %bb2 ], [ %b, %bb1 ], [%b, %default]
  %cmp4 = icmp ult i32 %phi1, %a
  ret i1 %cmp4
}

!1 = !{i1 1}
