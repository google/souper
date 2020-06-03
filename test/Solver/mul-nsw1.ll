

; RUN: %llvm-as -o %t %s
; RUN: %souper -check -souper-only-infer-i1 %t

define i32 @mul(i32 %a) #0 {
entry:
  %mul = mul nsw i32 %a, 3
  %cmp = icmp eq i32 %mul, 1, !expected !0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!0 = !{i1 0}
