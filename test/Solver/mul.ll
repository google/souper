; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %mul = mul nsw i32 %a, -2
  %cmp = icmp eq i32 %mul, 1, !expected !0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!0 = metadata !{ i1 0 }
