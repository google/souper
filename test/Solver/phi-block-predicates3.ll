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
  %phi1 = phi i32 [ 1, %entry ], [ 5, %foo1 ], [ 2, %foo2 ] 
  br label %label2
foo3:
  br label %label2
foo4:
  br label %label2
label2:
  %phi2 = phi i32 [ 2, %label1 ], [ 4, %foo3 ], [ %phi1, %foo4 ] 
  %cmp = icmp sle i32 %phi2, 4
  ret i1 %cmp
}
