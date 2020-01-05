; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-known-bits %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i8 %x2, i4 %x1, i64 %_phiinput) {
entry:

  %0 = zext i4 %x1 to i8
  %1 = lshr i8 %0, %x2
  ret i8 %1
}

; CHECK: %3:i8 = lshr %1, %2
; CHECK-NEXT: infer %3
; CHECK-NEXT: ; knownBits from souper: 0000xxxx
