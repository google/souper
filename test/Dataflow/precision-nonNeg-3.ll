; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-non-neg %t > %t2 || true
; RUN: %FileCheck %s < %t2

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@glob_x2_and = external global i64
@glob_x2_or = external global i64
@glob_x2_negative = external global i64
@glob_x2_nonNegative = external global i64
@glob_x2_powerOfTwo = external global i64
@glob_x2_nonZero = external global i64
@glob_x2_signBits = external global i64
@glob_x2_range = external global i64
@glob_x1_and = external global i64
@glob_x1_or = external global i64
@glob_x1_negative = external global i64
@glob_x1_nonNegative = external global i64
@glob_x1_powerOfTwo = external global i64
@glob_x1_nonZero = external global i64
@glob_x1_signBits = external global i64
@glob_x1_range = external global i64

define i64 @foo(i64 %t8_x2, i64 %t8_x1, i64 %_phiinput) {
entry:
  %ptr_1 = alloca i64
  store i64 %t8_x1, i64* %ptr_1
  call void @fun_1(i64* %ptr_1)
  %x1 = load i64, i64* %ptr_1, !range !1
  store i64 %x1, i64* @glob_x1_range
  %ptr_0 = alloca i64
  store i64 %t8_x2, i64* %ptr_0
  call void @fun_0(i64* %ptr_0)
  %x2 = load i64, i64* %ptr_0, !range !0
  store i64 %x2, i64* @glob_x2_range

  %0 = icmp ult i64 %x2, %x1
  %1 = select i1 %0, i64 %x2, i64 %x1
  %2 = sub nsw i64 %x1, %1
  ret i64 %2
}
!0 = !{ i64 1, i64 0 }
!1 = !{ i64 1, i64 -9223372036854775808 }
declare void @fun_0(i64* %ptr_0)
declare void @fun_1(i64* %ptr_1)

; CHECK: %4:i64 = subnsw %0, %3
; CHECK-NEXT: infer %4
; CHECK-NEXT: ; nonNegative from souper: true
