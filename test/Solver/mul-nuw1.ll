

; RUN: %llvm-as -o %t %s
; RUN: %souper -check -souper-only-infer-i1 %t

define i32 @foo(i32 %x) #0 {
entry:
  %mul = mul nuw i32 %x, %x
  %cmp = icmp uge i32 %mul, %x, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = !{i1 1}
