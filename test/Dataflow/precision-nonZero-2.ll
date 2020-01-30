; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -infer-non-zero %t > %t2 || true
; RUN: %FileCheck %s < %t2


define i64 @foo(i16 %x1, i64 %_phiinput) {
entry:

  %0 = zext i16 %x1 to i64
  %1 = add nsw nuw i64 128, %0
  %2 = lshr i64 %1, 8
  %3 = sub nsw i64 %1, %2
  ret i64 %3
}

; CHECK: %4:i64 = subnsw %2, %3
; CHECK-NEXT: infer %4
; CHECK-NEXT: ; nonZero from souper: true
