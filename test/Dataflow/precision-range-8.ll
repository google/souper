; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i8 %x1, i64 %_phiinput) {
entry:

  %0 = udiv i8 20, %x1
  ret i8 %0
}
; CHECK: %1:i8 = udiv 20:i8, %0
; CHECK-NEXT: infer %1
; CHECK-NEXT: ; range from souper: [0,21)
