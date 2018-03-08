; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a) {
entry:
  br label %label1
foo1:
  br label %label1
foo2:
  br label %label1
label1:
  %phi1 = phi i32 [ 1, %entry ], [ 2, %foo1 ], [ 3, %foo2 ] 
  %phi2 = phi i32 [ 2, %entry ], [ 4, %foo1 ], [ 6, %foo2 ] 
  %res = mul nsw i32 %phi1, 2
  %cmp = icmp eq i32 %res, %phi2, !expected !1 
  ret i1 %cmp
}

!1 = !{i1 1}
