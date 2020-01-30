; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-neg %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i1 @foo(i64 %x1, i64 %_phiinput) {
entry:

  %sw0 = icmp eq i64 %_phiinput, 0
  br i1 %sw0 , label %nl0, label %foo0
nl0:
  br label %foo1
foo0:
  br label %philabel0
foo1:
  br label %philabel0
philabel0:
  %0 = phi i64 [%x1, %foo0], [0, %foo1]
  %1 = icmp eq i64 %0, %0
  ret i1 %1
}

; CHECK: %3:i1 = eq %2, %2
; CHECK-NEXT: infer %3
; CHECK-NEXT: ; negative from souper: true
