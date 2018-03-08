; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i32 @foo(i32 %x, i32 %y) #0 {
entry:
  %shl = shl nuw i32 %x, %y
  %shr = lshr i32 %shl, %y
  %cmp = icmp eq i32 %shr, %x, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = !{i1 1}
