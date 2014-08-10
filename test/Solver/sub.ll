; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %sub = sub nuw i32 0, %a
  %sub1 = sub nuw i32 %sub, %b
  %cmp = icmp sle i32 %sub1, 0, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = metadata !{ i1 1 }
