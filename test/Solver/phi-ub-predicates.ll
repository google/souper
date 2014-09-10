; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a) #0 {
entry:
  %0 = sub nsw i32 2147483647, %a
  %1 = icmp slt i32 %0, 1
  br label %phi
foo:
  br label %phi
phi:
  %2 = phi i1 [ 0, %foo ], [ %1, %entry ]
  %3 = zext i1 %2 to i32
  %4 = add nsw i32 7, %a
  %5 = select i1 %2, i32 %3, i32 %4
  %res = icmp eq i32 0, %5
  ret i1 %res
}

!0 = metadata !{ i1 0 }
