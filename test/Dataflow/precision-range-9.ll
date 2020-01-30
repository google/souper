; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i32 @foo(i32 %x3, i32 %x4, i32 %x1, i32 %x2, i64 %_phiinput) {
entry:

  %0 = icmp eq i32 4294967295, %x4
  %1 = icmp eq i32 22, %x3
  %2 = select i1 %0, i32 0, i32 1
  %3 = or i32 2, %2
  %4 = icmp eq i32 4294967295, %x2
  %5 = icmp eq i32 22, %x1
  %6 = select i1 %1, i32 %2, i32 %3
  %7 = or i32 4, %6
  %8 = select i1 %4, i32 %6, i32 %7
  %9 = or i32 8, %8
  %10 = select i1 %5, i32 %8, i32 %9
  ret i32 %10
}

; CHECK: %14:i32 = select %1, %12, %13
; CHECK-NEXT: infer %14
; CHECK-NEXT: ; range from souper: [0,16)
