; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-power-two %t > %t2 || true
; RUN: %FileCheck %s < %t2

@glob_x1_and = external global i8
@glob_x1_or = external global i8
@glob_x1_negative = external global i8
@glob_x1_nonNegative = external global i8
@glob_x1_powerOfTwo = external global i8
@glob_x1_nonZero = external global i8
@glob_x1_signBits = external global i8
@glob_x1_range = external global i8

define i8 @foo(i8 %t8_x1, i64 %_phiinput) {
entry:
  %ptr_0 = alloca i8
  store i8 %t8_x1, i8* %ptr_0
  call void @fun_0(i8* %ptr_0)
  %x1 = load i8, i8* %ptr_0, !range !0
  store i8 %x1, i8* @glob_x1_range

  ret i8 %x1
}
!0 = !{ i8 1, i8 3 }

declare void @fun_0(i8* %ptr_0)

; CHECK: infer %0
; CHECK: ; powerOfTwo from souper: true
