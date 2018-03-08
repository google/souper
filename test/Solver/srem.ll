; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i32 @foo(i32 %x) {
entry:
  %srem = srem i32 %x, 9
  %a = icmp slt i32 %srem, 9, !expected !1
  %conv = zext i1 %a to i32
  ret i32 %conv
}

!1 = !{i1 1}
