; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @fn1(i32 %a) #0 {
entry:
  %div = udiv i32 1999999973, %a
  %cmp1 = icmp eq i32 %div, 1
  %cmp2 = icmp eq i32 %div, 1999999973
  %c = or i1 %cmp1, %cmp2
  %conv = zext i1 %c to i32
  ret i32 %conv
}
