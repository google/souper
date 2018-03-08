; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i16 @fn1(i16 %a) #0 {
entry:
  %div = udiv exact i16 65497, %a
  %cmp1 = icmp eq i16 %div, 1
  %cmp2 = icmp eq i16 %div, 65497
  %c = or i1 %cmp1, %cmp2, !expected !1
  %conv = zext i1 %c to i16
  ret i16 %conv
}

!1 = !{i1 1}
