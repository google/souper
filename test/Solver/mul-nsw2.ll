; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define void @mul(i16 %a) #0 {
entry:
  %nega = sub i16 0, %a
  %mul = mul nsw i16 %a, %nega
  %cmp1 = icmp sgt i16 %mul, %a, !expected !0
  %cmp2 = icmp sgt i16 %mul, %nega, !expected !0
  ret void
}

!0 = metadata !{ i1 0 }
