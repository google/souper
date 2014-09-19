; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a) {
entry:
  br label %phi
foo:
  br label %phi
foo2:
  br label %phi
phi:
  %phi1 = phi i32 [ 1, %entry ], [ 5, %foo ], [ 2, %foo2 ] 
  br label %foo5
foo3:
  br label %foo5
foo4:
  br label %foo5
foo5:
  %phi2 = phi i32 [ 2, %phi ], [ 4, %foo3 ], [ %phi1, %foo4 ] 
  %cmp = icmp sle i32 %phi2, 4
  ret i1 %cmp
}
