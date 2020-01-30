; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-range %t > %t2 || true
; RUN: %FileCheck %s < %t2

define i64 @foo(i32 %x1, i64 %_phiinput) {
entry:

  %0 = zext i32 %x1 to i64
  %1 = icmp ult i32 %x1, 53
  %tmp0 = icmp eq i1 1, %1
  br i1 %tmp0, label %cont0, label %out0
out0:
  unreachable
cont0:
  %2 = add i64 %0, 0
  ret i64 %2
}
; CHECK: pc %2 1:i1
; CHECK-NEXT: %3:i64 = zext %0
; CHECK-NEXT: %4:i64 = add 0:i64, %3
; CHECK-NEXT: infer %4
; CHECK-NEXT: ; range from souper: [0,53)
