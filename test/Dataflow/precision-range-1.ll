; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @foo(i8 %x1, i64 %_phiinput) {
entry:

  %0 = icmp eq i8 0, %x1
  %1 = select i1 %0, i8 1, i8 %x1
  ret i8 %1
}

; CHECK: %2:i8 = select %1, 1:i8, %0
; CHECK-NEXT: infer %2
; CHECK-NEXT: ; range from souper: [1,0)
