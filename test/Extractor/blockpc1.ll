; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind uwtable
define i32 @foo() #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.body, %entry
  %i.0 = phi i32 [ undef, %entry ], [ %inc1, %for.body ]
  %len.0 = phi i32 [ undef, %entry ], [ %inc, %for.body ]
  %tobool = icmp eq i32 %i.0, 0
  br i1 %tobool, label %for.end, label %for.body

for.body:                                         ; preds = %for.cond
  %inc = add i32 %len.0, 1
  %inc1 = add i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %tobool2 = icmp eq i32 %len.0, 0
  br i1 %tobool2, label %if.end, label %if.then

if.then:                                          ; preds = %for.end
  %inc3 = add i32 %len.0, 1
  br label %if.end

if.end:                                           ; preds = %for.end, %if.then
  %len.1 = phi i32 [ %inc3, %if.then ], [ %len.0, %for.end ]
  %tobool4 = icmp eq i32 %len.1, 0
  br i1 %tobool4, label %if.end6, label %if.then5

if.then5:                                         ; preds = %if.end
  br label %return

if.end6:                                          ; preds = %if.end
  br label %return

return:                                           ; preds = %if.end6, %if.then5
  %retval.0 = phi i32 [ 1, %if.then5 ], [ 0, %if.end6 ]
  ret i32 %retval.0
}

