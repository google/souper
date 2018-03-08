
; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind uwtable
define void @fn1() #0 {
entry:
  br i1 true, label %if.then, label %if.end12

if.then:                                          ; preds = %entry
  br i1 true, label %if.then2, label %if.end

if.then2:                                         ; preds = %if.then
  %0 = add i32 0, 0
  br label %if.end

if.end:                                           ; preds = %if.then2, %if.then
  %tt.0 = phi i32 [ %0, %if.then2 ], [ undef, %if.then ]
  br i1 true, label %if.then5, label %if.end7

if.then5:                                         ; preds = %if.end
  %1 = add i32 0, 0
  br label %if.end7

if.end7:                                          ; preds = %if.then5, %if.end
  %tt.1 = phi i32 [ %1, %if.then5 ], [ %tt.0, %if.end ]
  %tobool8 = icmp eq i32 %tt.1, 0, !expected !1
  br i1 %tobool8, label %if.end12, label %if.then9

if.then9:                                         ; preds = %if.end7
  br label %if.end12

if.end12:                                         ; preds = %if.end7, %if.then9, %entry
  ret void
}

!1 = !{i1 1}
