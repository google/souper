; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-sign-bits %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i64 @foo(i16 %x1, i64 %_phiinput) {
entry:

  %0 = zext i16 %x1 to i64
  %1 = add nsw nuw i64 128, %0
  %2 = lshr i64 %1, 8
  %3 = sub nsw i64 %1, %2
  %4 = lshr i64 %3, 8
  ret i64 %4
}

; CHECK: %5:i64 = lshr %4, 8:i64
; CHECK-NEXT: infer %5
; CHECK-NEXT; signBits from souper: 56
