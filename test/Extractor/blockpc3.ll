; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

@x1 = common global i32 0, align 4
@x0 = common global i32 0, align 4
@x5 = common global i32 0, align 4
@x2 = common global i32 0, align 4
@x3 = common global i32 0, align 4
@x4 = common global i32 0, align 4
@str = private unnamed_addr constant [2 x i8] c"x\00", align 1

define i32 @main() {
  %1 = load i32* @x1
  %2 = icmp slt i32 %1, 1
  br i1 %2, label %._crit_edge, label %3

._crit_edge:                                      ; preds = %0
  %.pre = load i32* @x2
  br label %14

; <label>:3                                       ; preds = %0
  %4 = load i32* @x0
  %5 = icmp sgt i32 %4, 1
  %6 = zext i1 %5 to i32
  br i1 %5, label %10, label %7

; <label>:7                                       ; preds = %3
  %8 = icmp sgt i32 %4, 0
  %9 = zext i1 %8 to i32
  br label %10

; <label>:10                                      ; preds = %7, %3
  %11 = phi i32 [ %9, %7 ], [ %6, %3 ]
  store i32 %11, i32* @x5
  store i32 %11, i32* @x2
  %12 = icmp slt i32 %1, %11, !expected !0
  br i1 %12, label %14, label %13

; <label>:13                                      ; preds = %10
  store i32 0, i32* @x3
  br label %14

; <label>:14                                      ; preds = %13, %10, %._crit_edge
  %15 = phi i32 [ %11, %10 ], [ %.pre, %._crit_edge ], [ %11, %13 ]
  store i32 %15, i32* @x4
  %16 = icmp eq i32 %1, 0
  br i1 %16, label %17, label %18

; <label>:17                                      ; preds = %14
  %puts = tail call i32 @puts(i8* getelementptr inbounds ([2 x i8]* @str, i64 0, i64 0))
  br label %18

; <label>:18                                      ; preds = %17, %14
  ret i32 0
}

declare i32 @puts(i8*)

!0 = metadata !{ i1 0 }

