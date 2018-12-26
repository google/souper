; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t 2> %t2 || true
; RUN: %FileCheck %s < %t2

define i1 @cmp_with_range(i8*, i8*) {
  %v1 = load i8, i8* %0, !range !0
  %v2 = load i8, i8* %1, !range !1
  %v3 = shl i8 %v1, %v2
  ; CHECK: instruction:
  ; CHECK-NEXT: %out = icmp ule i8 %v3, 32
  ; CHECK-NEXT: unexpected simplification:
  ; CHECK-NEXT: ; Function: cmp_with_range
  ; CHECK-NEXT: %0:i8 = var (knownBits=000000xx) (nonNegative) (nonZero) (signBits=6) (range=[1,3)
  ; CHECK-NEXT: %1:i8 = var (knownBits=00000xxx) (nonNegative) (nonZero) (signBits=5) (range=[1,5)
  ; CHECK-NEXT: %2:i8 = shl %0, %1
  ; CHECK-NEXT: %3:i1 = ule %2, 32:i8
  ; CHECK-NEXT: cand %3 1:i1
  %out = icmp ule i8 %v3, 32
  ret i1 %out
}

!0 = !{i8 1, i8 3}
!1 = !{i8 1, i8 5}
