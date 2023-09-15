; RUN: %souper --souper-enumerative-synthesis-max-instructions=1 -souper-mark-external-uses=true %s 2>&1 | %FileCheck --check-prefix="EXTERNAL_USES_ON" %s
; RUN: %souper --souper-enumerative-synthesis-max-instructions=1 -souper-mark-external-uses=false %s 2>&1 | %FileCheck --check-prefix="EXTERNAL_USES_OFF" %s

; EXTERNAL_USES_ON-NOT: cand

; EXTERNAL_USES_OFF: cand
; EXTERNAL_USES_OFF: cand

define i32 @foo(i32, i32) {
  %3 = sub i32 0, %0
  %4 = sub i32 0, %1
  %5 = sub i32 %3, %4
  %6 = add i32 %3, %4
  %7 = xor i32 %5, %6
  ret i32 %7
}
