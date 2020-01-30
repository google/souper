; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-neg %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i1 @foo(i32 %x1, i64 %_phiinput) {
entry:

  %0 = mul nsw i32 10, %x1
  %1 = srem i32 %0, 10
  %2 = icmp eq i32 0, %1
  ret i1 %2
}

; CHECK: %3:i1 = eq 0:i32, %2
; CHECK-NEXT: infer %3
; CHECK-NEXT: ; negative from souper: true
