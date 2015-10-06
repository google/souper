; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -souper-exploit-blockpcs -check %t

define i32 @foo(i32 %x) {
entry:
%cmp1 = icmp eq i32 %x, 0
br i1 %cmp1, label %cond1, label %cond2

cond1:                   ; preds = %entry
%x10 = add nsw i32 10, %x
br label %phi1

cond2:                   ; preds = %entry
br label %phi1

phi1:                    ; preds = %cond1, %cond2
%r = phi i32 [10, %cond2], [%x10, %cond1]
%cmp2 = icmp eq i32 %r, 10, !expected !1
br i1 %cmp2, label %cond3, label %cond4

cond3:                   ; preds = %phi1
%cmp10 = zext i1 %cmp2 to i32
%t1 = add nsw i32 %cmp10, 0
br label %phi2

cond4:                   ; preds = %phi1
%t2 = add nsw i32 0, 1
br label %phi2

phi2:                    ; preds = %cond3, %cond4
%t3 = phi i32 [%t1, %cond3], [%t2, %cond4]
%cmp4 = icmp eq i32 %t3, 1, !expected !1
%conv = zext i1 %cmp4 to i32
ret i32 %conv
}

!1 = !{i1 1}

