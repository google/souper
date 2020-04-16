; REQUIRES: solver

; RUN: %opt -O2 -S -o - %s | %FileCheck -check-prefix=TEST1 %s
; TEST1: add i32 %in, 5

; RUN: %opt -disable-all-peepholes -O2 -S -o - %s | %FileCheck -check-prefix=TEST2 %s
; TEST2-NOT: add i32 %in, 5

define i32 @func(i32 %in) {
  %x1 = add i32 %in, 1
  %x2 = add i32 %x1, 1
  %x3 = add i32 %x2, 1
  %x4 = add i32 %x3, 1
  %x5 = add i32 %x4, 1
  ret i32 %x5
}
