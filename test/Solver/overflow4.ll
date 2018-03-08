; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; ModuleID = 'overflow4.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.src = private unnamed_addr constant [12 x i8] c"overflow4.c\00", align 1
@0 = private unnamed_addr constant { i16, i16, [6 x i8] } { i16 0, i16 11, [6 x i8] c"'int'\00" }
@1 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 3, i32 56 }, { i16, i16, [6 x i8] }* @0 }

; Function Attrs: nounwind uwtable
define i32 @x1(i32 %x0) #0 {
entry:
  %cmp = icmp ult i32 %x0, 1000
  br i1 %cmp, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  %mul = mul i32 %x0, 1000
  br label %cond.end

cond.false:                                       ; preds = %entry
  %0 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 0, i32 1)
  %1 = extractvalue { i32, i1 } %0, 0
  %2 = extractvalue { i32, i1 } %0, 1, !expected !3
  %3 = xor i1 %2, true, !nosanitize !1, !expected !4
  br i1 %3, label %cont, label %handler.negate_overflow, !prof !2, !nosanitize !1

handler.negate_overflow:                          ; preds = %cond.false
  call void @__ubsan_handle_negate_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* }* @1 to i8*), i64 1) #3, !nosanitize !1
  br label %cont, !nosanitize !1

cont:                                             ; preds = %handler.negate_overflow, %cond.false
  br label %cond.end

cond.end:                                         ; preds = %cont, %cond.true
  %cond = phi i32 [ %mul, %cond.true ], [ %1, %cont ]
  ret i32 %cond
}

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32) #1

; Function Attrs: uwtable
declare void @__ubsan_handle_negate_overflow(i8*, i64) #2

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }
attributes #2 = { uwtable }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.7.0 (trunk 228761)"}
!1 = !{}
!2 = !{!"branch_weights", i32 1048575, i32 1}
!3 = !{i1 0}
!4 = !{i1 1}
