; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a) {
entry:
  %0 = add nsw i32 %a, 1
  br label %phi
foo:
  %1 = add i32 %a, 1
  br label %phi
phi:
  %2 = phi i32 [ %0, %entry ], [ %1, %foo ] 
  %res = icmp sgt i32 %2, %a
  ret i1 %res
}
