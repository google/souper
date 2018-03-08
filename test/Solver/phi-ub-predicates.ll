; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a) {
entry:
  %0 = sub nsw i32 2147483647, %a
  %1 = icmp slt i32 %0, 1
  br label %label1
foo1:
  br label %label1
label1:
  %2 = phi i1 [ 0, %foo1 ], [ %1, %entry ]
  %3 = zext i1 %2 to i32
  %4 = add nsw i32 7, %a
  %5 = select i1 %2, i32 %3, i32 %4
  %res = icmp eq i32 0, %5
  ret i1 %res
}
