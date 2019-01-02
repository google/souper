; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @cmp_with_range(i8*, i8*) {
  %v1 = load i8, i8* %0, !range !0
  %v2 = load i8, i8* %1, !range !1
  %out = icmp eq i8 %v1, %v2, !expected !2
  ret i1 %out
}

!0 = !{i8 5, i8 7}
!1 = !{i8 0, i8 2}
!2 = !{i1 0 }
