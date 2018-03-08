
; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define zeroext i1 @c_ispunct(i32 %a, i32 %b, i32 %c) {
entry:
  switch i32 %a, label %default [
    i32 0, label %bb2
    i32 1, label %bb3
  ]

default:                                    ; preds = %entry
  %cmp1 = icmp eq i32 %c, 0
  br i1 %cmp1, label %bb1, label %return

bb1:                                        ; preds = %default
  %inc = add i32 %a, 1
  br label %bb4

bb2:                                        ; preds = %entry
  br label %bb4

bb3:                                        ; preds = %entry
  %cmp2 = icmp eq i32 1, %a, !expected !1
  br i1 %cmp2, label %bb2, label %bb4

bb4:                                        ; preds = %bb1, %bb2, %bb3
  %phi1 = phi i32 [ %b, %bb3 ], [ %b, %bb2 ], [ %inc, %bb1 ]
  %cmp4 = icmp ult i32 %phi1, %a
  br label %return

return:                                     ; preds = %bb4, %default
  %retval.0 = phi i1 [ %cmp4, %bb4 ], [ %cmp1, %default ]
  ret i1 %retval.0
}

!1 = !{i1 1}
