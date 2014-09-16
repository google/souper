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
  %phi1 = phi i32 [ 1, %entry ], [ 2, %foo ], [ 3, %foo2 ] 
  br label %foo5
foo3:
  br label %foo5
foo4:
  br label %foo5
foo5:
  %phi2 = phi i32 [ 2, %phi ], [ 4, %foo3 ], [ 6, %foo4 ] 
  %res = mul nsw i32 %phi1, 2
  %cmp = icmp eq i32 %res, %phi2
  ret i1 %cmp
}
