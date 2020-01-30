; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-sign-bits %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i32 @foo(i8 %x1, i8 %x2, i64 %_phiinput) {
entry:

  %0 = zext i8 %x2 to i32
  %1 = zext i8 %x1 to i32
  %2 = sub nsw i32 %1, %0
  %3 = mul nsw i32 %2, %2
  ret i32 %3
}
; CHECK: %5:i32 = mulnsw %4, %4
; CHECK-NEXT: infer %5
; CHECK-NEXT: ; signBits from souper: 16
