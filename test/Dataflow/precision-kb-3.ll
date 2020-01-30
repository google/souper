; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-known-bits %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i8 %x1, i64 %_phiinput) {
entry:

  %0 = shl i8 32, %x1
  ret i8 %0
}

; CHECK: %1:i8 = shl 32:i8, %0
; CHECK-NEXT: infer %1
; CHECK-NEXT: ; knownBits from souper: xxx00000
