; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @fermat(i20 %a, i20 %b, i20 %c) #0 {
entry:
  %ainc = add nuw i20 %a, 1
  %asqr = mul nuw i20 %ainc, %ainc
  %acub = mul nuw i20 %ainc, %asqr
  %binc = add nuw i20 %b, 1
  %bsqr = mul nuw i20 %binc, %binc
  %bcub = mul nuw i20 %binc, %bsqr
  %cinc = add nuw i20 %c, 1
  %csqr = mul nuw i20 %cinc, %cinc
  %ccub = mul nuw i20 %cinc, %csqr
  %abcub = add nuw i20 %acub, %bcub
  %cmp = icmp eq i20 %abcub, %ccub, !expected !0
  ret i1 %cmp
}

!0 = !{i1 0}
