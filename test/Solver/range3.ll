; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t 2> %t2 || true
; RUN: %FileCheck %s < %t2

define i8 @cmp_with_range(i8*) {
  %v1 = load i8, i8* %0, !range !0
  %v2 = shl i8 2, %v1
  ; CHECK: instruction:
  ; CHECK-NEXT: %v3 = urem i8 %v2, 2
  ; CHECK-NEXT: unexpected simplification:
  ; CHECK-NEXT: ; Function: cmp_with_range
  ; CHECK-NEXT: %0:i8 = var (knownBits=000000xx) (nonNegative) (nonZero) (signBits=6) (range=[1,4)
  ; CHECK-NEXT: %1:i8 = shl 2:i8, %0
  ; CHECK-NEXT: %2:i8 = urem %1, 2:i8
  ; CHECK-NEXT: cand %2 0:i8
  %v3 = urem i8 %v2, 2
  ret i8 %v3
}

!0 = !{i8 1, i8 4}
