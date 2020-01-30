; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@glob_x1_and = external global i32
@glob_x1_or = external global i32
@glob_x1_negative = external global i32
@glob_x1_nonNegative = external global i32
@glob_x1_powerOfTwo = external global i32
@glob_x1_nonZero = external global i32
@glob_x1_signBits = external global i32
@glob_x1_range = external global i32

define i32 @foo(i32 %t8_x1, i64 %_phiinput) {
entry:
  %ptr_0 = alloca i32
  store i32 %t8_x1, i32* %ptr_0
  call void @fun_0(i32* %ptr_0)
  %t4_x1 = load i32, i32* %ptr_0, !range !0
  store i32 %t4_x1, i32* @glob_x1_range

  %0 = xor i32 4294967295, %t4_x1
  ret i32 %0
}
!0 = !{ i32 0, i32 2147483647 }

declare void @fun_0(i32* %ptr_0)

; CHECK: %1:i32 = xor 4294967295:i32, %0
; CHECK-NEXT: infer %1
; CHECK-NEXT: ; range from souper: [-2147483647,0)
