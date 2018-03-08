; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i256 @foo(i256 %a, i256 %b) #0 {
entry:
  %sub = sub nuw i256 0, %a
  %sub1 = sub nuw i256 %sub, %b
  %cmp = icmp eq i256 %sub1, 0, !expected !1
  %conv = zext i1 %cmp to i256
  ret i256 %conv
}

!1 = !{i1 1}
