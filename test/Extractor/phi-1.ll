
; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@a = common global i32* null, align 8

; Function Attrs: nounwind uwtable
define i32 @fn1() #0 {
entry:
  br i1 false, label %cond.end, label %cond.false

cond.false:                                       ; preds = %entry
  %0 = load i32*, i32** @a, align 8
  %1 = load i32, i32* %0, align 4
  br label %cond.end

cond.end:                                         ; preds = %entry, %cond.false
  %cond = phi i32 [ %1, %cond.false ], [ undef, %entry ]
  br label %for.cond

for.cond:                                         ; preds = %for.cond, %cond.end
  %tobool1 = icmp eq i32 %cond, 0
  br i1 %tobool1, label %for.end, label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 undef
}

