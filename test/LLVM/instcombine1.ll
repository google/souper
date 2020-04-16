; REQUIRES: solver

; RUN: %opt -O2 -S -o - %s | %FileCheck -check-prefix=TEST1 %s
; TEST1: %c = xor i40 %c.demorgan, -1

; RUN: %opt -disable-all-peepholes -O2 -S -o - %s | %FileCheck -check-prefix=TEST2 %s
; TEST2: and i40 %a, %b

define i40 @func(i40 %in1, i40 %in2) {
  %a = xor i40 %in1, -1
  %b = xor i40 %in2, -1
  %c = and i40 %a, %b
  ret i40 %c
}
