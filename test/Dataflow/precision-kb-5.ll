; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-known-bits %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i32 @foo(i16 %x3, i16 %x1, i16 %x2, i64 %_phiinput) {
entry:

  %0 = zext i16 %x1 to i32
  %1 = add nsw nuw i32 2, %0
  %2 = zext i16 %x2 to i32
  %3 = shl nsw nuw i32 %2, 1
  %4 = add nsw nuw i32 %1, %3
  %5 = zext i16 %x3 to i32
  %6 = add nsw nuw i32 %4, %5
  ret i32 %6
}

; CHECK: %9:i32 = addnw %6, %8
; CHECK-NEXT: infer %9
; CHECK-NEXT: ; knownBits from souper: 00000000000000xxxxxxxxxxxxxxxxxx
