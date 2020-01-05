; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i8 %x1, i64 %_phiinput) {
entry:

  %0 = add nuw i8 1, %x1
  ret i8 %0
}

; CHECK: %1:i8 = addnuw 1:i8, %0
; CHECK-NEXT: infer %1
; CHECK-NEXT: ; range from souper: [1,0)
