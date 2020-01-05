; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-non-zero %t > %t2 || true
; RUN: %FileCheck %s < %t2

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@glob_x1_and = external global i64
@glob_x1_or = external global i64
@glob_x1_negative = external global i64
@glob_x1_nonNegative = external global i64
@glob_x1_powerOfTwo = external global i64
@glob_x1_nonZero = external global i64
@glob_x1_signBits = external global i64
@glob_x1_range = external global i64

define i64 @foo(i64 %t8_x1, i64 %_phiinput) {
entry:
  %ptr_0 = alloca i64
  store i64 %t8_x1, i64* %ptr_0
  call void @fun_0(i64* %ptr_0)
  %x1 = load i64, i64* %ptr_0, !range !0
  store i64 %x1, i64* @glob_x1_range

  %0 = add nuw i64 1, %x1
  ret i64 %0
}
!0 = !{ i64 0, i64 -1 }

declare void @fun_0(i64* %ptr_0)

; CHECK: %1:i64 = addnuw 1:i64, %0
; CHECK-NEXT: infer %1
; CHECK-NEXT: ; nonZero from souper: true
