; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i8 @cmp_with_range(i8*) {
  %v1 = load i8, i8* %0, !range !0
  %v2 = shl i8 2, %v1
  %v3 = urem i8 %v2, 2, !expected !1
  ret i8 %v3
}

!0 = !{i8 1, i8 4}
!1 = !{i8 0}
