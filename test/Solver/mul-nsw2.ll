; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare void @sink(i1) nounwind readnone

define void @mul(i12 %a) #0 {
entry:
  %nega = sub i12 0, %a
  %mul = mul nsw i12 %a, %nega
  %cmp1 = icmp sgt i12 %mul, %a, !expected !0
  %cmp2 = icmp sgt i12 %mul, %nega, !expected !0
  call void @sink(i1 %cmp1)
  call void @sink(i1 %cmp2)
  ret void
}

!0 = !{i1 0}
