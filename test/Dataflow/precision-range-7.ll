; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i32 @foo(i32 %x1, i64 %_phiinput) {
entry:

  %0 = shl i32 1, %x1
  %1 = and i32 6, %0
  ret i32 %1
}
; CHECK: %2:i32 = and 6:i32, %1
; CHECK: infer %2
; CHECK-NEXT ; range from souper: [0,5)
