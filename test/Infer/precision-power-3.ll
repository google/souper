; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-power-two %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i32 %x1, i64 %_phiinput) {
entry:

  %0 = and i32 7, %x1
  %1 = shl i32 1, %0
  %2 = trunc i32 %1 to i8
  ret i8 %2
}

;CHECK: %3:i8 = trunc %2
;CHECK-NEXT: infer %3
;CHECK-NEXT: ; powerOfTwo from souper: true
