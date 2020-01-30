; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-non-neg %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i32 @foo(i32 %x2, i64 %x1, i64 %_phiinput) {
entry:

  %0 = lshr i32 %x2, 16
  %sw0 = icmp eq i64 %_phiinput, 0
  br i1 %sw0 , label %nl0, label %foo0
nl0:
  %sw1 = icmp eq i64 %_phiinput, 1
  br i1 %sw1 , label %nl1, label %foo1
nl1:
  br label %foo2
foo0:
  br label %philabel4
foo1:
  br label %philabel4
foo2:
  br label %philabel4
philabel4:
  %1 = phi i32 [%0, %foo0], [0, %foo1], [59, %foo2]
  %2 = trunc i64 %x1 to i32
  %3 = and i32 65535, %2
  %sw2 = icmp eq i64 %_phiinput, 2
  br i1 %sw2 , label %nl2, label %foo3
nl2:
  br label %foo4
foo3:
  br label %philabel0
foo4:
  br label %philabel0
philabel0:
  %4 = phi i32 [%3, %foo3], [%1, %foo4]
  ret i32 %4
}

; CHECK: %3:i32 = and 65535:i32, %2
; CHECK-NEXT: %4 = block 3
; CHECK-NEXT: %5:i32 = var
; CHECK-NEXT: %6:i32 = lshr %5, 16:i32
; CHECK-NEXT: %7:i32 = phi %4, %6, 0:i32, 59:i32
; CHECK-NEXT: %8:i32 = phi %0, %3, %7
; CHECK-NEXT: infer %8
; nonNegative from souper: true
