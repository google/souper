; REQUIRES: solver

; RUN: %souper %solver -check %t

define void @mul(i12 %a) #0 {
entry:
  %nega = sub i12 0, %a
  %mul = mul nsw i12 %a, %nega
  %cmp1 = icmp sgt i12 %mul, %a, !expected !0
  %cmp2 = icmp sgt i12 %mul, %nega, !expected !0
  ret void
}

!0 = !{i1 0}
