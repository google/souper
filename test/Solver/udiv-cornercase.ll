; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @foo(i32 %x) #0 {
entry:
  %div = udiv i32 -2147483648, -1
  %tobool = icmp ne i32 %div, 0, !expected !0
  br i1 %tobool, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %div, %cond.true ], [ 0, %cond.false ]
  ret i32 %cond
}

!0 = metadata !{ i1 0 }
