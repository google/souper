; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -souper-exploit-blockpcs -check %t

define i32 @foo(i32 %x) {
entry:
%cmp1 = icmp eq i32 %x, 0
br i1 %cmp1, label %cond1, label %cond2

cond1:
%t10 = add nsw i32 10, %x
br label %phi

cond2:
br label %phi

phi:
%r = phi i32 [10, %cond2], [%t10, %cond1]
%cmp = icmp eq i32 %r, 10, !expected !1
%conv = zext i1 %cmp to i32
ret i32 %conv
}

!1 = !{i1 1}

