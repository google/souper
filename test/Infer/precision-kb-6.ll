; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-known-bits %t > %t2 || true
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

define i1 @foo(i32 %t8_x1, i64 %_phiinput) {
entry:
  %ptr_0 = alloca i32
  store i32 %t8_x1, i32* %ptr_0
  call void @fun_0(i32* %ptr_0)
  %x1 = load i32, i32* %ptr_0, !range !0
  store i32 %x1, i32* @glob_x1_range

  %0 = icmp ne i32 0, %x1
  ret i1 %0
}
!0 = !{ i32 1, i32 0 }

declare void @fun_0(i32* %ptr_0)

; CHECK: %1:i1 = ne 0:i32, %0
; CHECK-NEXT: infer %1
; CHECK-NEXT: ; knownBits from souper: 1
