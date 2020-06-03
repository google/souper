; RUN: %llvm-as -o %t %s
; RUN: %souper -check %t 2> %t2 || true
; RUN: %FileCheck %s < %t2

define i1 @cmp_with_range(i8*, i8*) {
  %v1 = load i8, i8* %0, !range !0
  %v2 = load i8, i8* %1, !range !1
  %v3 = shl i8 %v1, %v2
  ; CHECK: instruction:
  ; CHECK-NEXT: %out = icmp ule i8 %v3, 32, !expected !2
  ; CHECK-NEXT: expected simplification, none found
  %out = icmp ule i8 %v3, 32, !expected !2
  ret i1 %out
}

!0 = !{i8 1, i8 4}
!1 = !{i8 1, i8 5}
!2 = !{i1 1}
