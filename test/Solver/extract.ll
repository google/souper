; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

%struct = type { i32, [4 x i8] }

define i32 @test1() {
  %A = extractvalue %struct { i32 2, [4 x i8] c"foo\00" }, 0
    ret i32 %A
    ; CHECK-LABEL: @test1(
    ; CHECK: ret i32 2
}

