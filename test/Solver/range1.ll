; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t 2> %t2 || true
; RUN: %FileCheck %s < %t2

define i1 @cmp_with_range(i8*, i8*) {
  %v1 = load i8, i8* %0, !range !0
  %v2 = load i8, i8* %1, !range !1
  ; CHECK: instruction:
  ; CHECK-NEXT: %out = icmp eq i8 %v1, %v2
  ; CHECK-NEXT: unexpected simplification:
  ; CHECK-NEXT: Function: cmp_with_range
  ; CHECK-NEXT: %0:i8 = var (knownBits=000001xx) (nonNegative) (nonZero) (signBits=5) (range=[5,7)
  ; CHECK-NEXT: %1:i8 = var (knownBits=0000000x) (nonNegative) (signBits=7) (range=[0,2)
  ; CHECK-NEXT: %2:i1 = eq %0, %1
  ; CHECK-NEXT: cand %2 0:i1
  %out = icmp eq i8 %v1, %v2
  ret i1 %out
}

!0 = !{i8 5, i8 7}
!1 = !{i8 0, i8 2}
