; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-only-infer-iN -souper-double-check %t

define i16 @fn1a() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = sdiv i16 1, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn2a() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = sdiv i16 -1, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn3a() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = sdiv i16 2000, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn4a() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = sdiv i16 -2000, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn5a() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = sdiv i16 1, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn6a(i16) {
  %d1 = sdiv i16 %0, 0, !expected !0
  ret i16 %d1
}

define i16 @fn1b() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = udiv i16 1, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn2b() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = udiv i16 -1, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn3b() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = udiv i16 2000, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn4b() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = udiv i16 -2000, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn5b() {
  %s1 = add i16 0, 0, !expected !0
  %d1 = udiv i16 1, %s1, !expected !0
  ret i16 %d1
}

define i16 @fn6b(i16) {
  %d1 = udiv i16 %0, 0, !expected !0
  ret i16 %d1
}

!0 = !{i16 0}
