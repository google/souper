; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i8 %x1, i64 %_phiinput) {
entry:

  %0 = and i8 1, %x1
  %1 = add i8 %x1, %0
  ret i8 %1
}

; CHECK: %2:i8 = add %0, %1
; CHECK: infer %2
; CHECK: ; range from souper: [{{0,-1|2,1}})
