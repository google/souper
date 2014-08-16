; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define void @fermat(i32 %a, i32 %b, i32 %c) #0 {
entry:
  %ainc = add nuw i32 %a, 1
  %asqr = mul nuw i32 %ainc, %ainc
  %acub = mul nuw i32 %ainc, %asqr
  %binc = add nuw i32 %b, 1
  %bsqr = mul nuw i32 %binc, %binc
  %bcub = mul nuw i32 %binc, %bsqr
  %cinc = add nuw i32 %c, 1
  %csqr = mul nuw i32 %cinc, %cinc
  %ccub = mul nuw i32 %cinc, %csqr
  %abcub = add nuw i32 %acub, %bcub
  %cmp = icmp eq i32 %abcub, %ccub, !expected !0
  ret void
}

!0 = metadata !{ i1 0 }
