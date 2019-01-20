; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @fn1(i32 %x, i32 %y) #0 {
entry:
  %shr = ashr exact i32 %x, %y
  %a = icmp ne i32 %x, 0
  %b = icmp ne i32 %shr, 0
  %c = xor i1 %a, %b, !expected !0
  ret i1 %c
}

!0 = !{i1 0}
