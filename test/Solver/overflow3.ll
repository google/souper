; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

; ModuleID = 'overflow3.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@x0 = common global i32 0, align 4
@.src = private unnamed_addr constant [12 x i8] c"overflow3.c\00", align 1
@0 = private unnamed_addr constant { i16, i16, [6 x i8] } { i16 0, i16 11, [6 x i8] c"'int'\00" }
@1 = private unnamed_addr global { { [12 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* } { { [12 x i8]*, i32, i32 } { [12 x i8]* @.src, i32 2, i32 26 }, { i16, i16, [6 x i8] }* @0 }

; Function Attrs: nounwind uwtable
define i32 @x1() #0 {
entry:
  %0 = load i32, i32* @x0, align 4
  %1 = call { i32, i1 } @llvm.smul.with.overflow.i32(i32 %0, i32 1000)
  %2 = extractvalue { i32, i1 } %1, 0
  %3 = extractvalue { i32, i1 } %1, 1
  %4 = xor i1 %3, true, !nosanitize !1
  br i1 %4, label %cont, label %handler.mul_overflow, !prof !2, !nosanitize !1

handler.mul_overflow:                             ; preds = %entry
  %5 = zext i32 %0 to i64, !nosanitize !1
  call void @__ubsan_handle_mul_overflow(i8* bitcast ({ { [12 x i8]*, i32, i32 }, { i16, i16, [6 x i8] }* }* @1 to i8*), i64 %5, i64 1000) #3, !nosanitize !1
  br label %cont, !nosanitize !1

cont:                                             ; preds = %handler.mul_overflow, %entry
  ret i32 %2
}

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.smul.with.overflow.i32(i32, i32) #1

; Function Attrs: uwtable
declare void @__ubsan_handle_mul_overflow(i8*, i64, i64) #2

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }
attributes #2 = { uwtable }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.7.0 (trunk 228761)"}
!1 = !{}
!2 = !{!"branch_weights", i32 1048575, i32 1}
