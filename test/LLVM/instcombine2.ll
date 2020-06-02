

; RUN: %opt -O2 -S -o - %s | %FileCheck -check-prefix=TEST1 %s
; TEST1: ret i60 %in2

; RUN: %opt -disable-all-peepholes -O2 -S -o - %s | %FileCheck -check-prefix=TEST2 %s
; TEST2: %a = xor i60 %in1, %in2

define i60 @func(i60 %in1, i60 %in2) {
  %a = xor i60 %in1, %in2
  %b = xor i60 %in1, %a
  ret i60 %b
}
