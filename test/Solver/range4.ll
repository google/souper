; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @cmp_with_range1(i64*, i64*) {
  %v1 = load i64, i64* %0, !range !0
  %out = icmp eq i64 %v1, 98
  ret i1 %out
}

define i1 @cmp_with_range2(i64*, i64*) {
  %v1 = load i64, i64* %0, !range !0
  %out = icmp eq i64 %v1, 99, !expected !1
  ret i1 %out
}

define i1 @cmp_with_range3(i64*, i64*) {
  %v1 = load i64, i64* %0, !range !0
  %out = icmp eq i64 %v1, 100
  ret i1 %out
}

define i1 @cmp_with_range4(i64*, i64*) {
  %v1 = load i64, i64* %0, !range !0
  %out = icmp eq i64 %v1, 101
  ret i1 %out
}

!0 = !{i64 100, i64 99}
!1 = !{i1 0}
