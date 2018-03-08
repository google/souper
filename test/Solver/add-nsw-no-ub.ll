; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -souper-exploit-ub=false -check %t

define i32 @foo(i32 %x) {
entry:
  %add = add nsw i32 %x, 1
  %cmp = icmp sgt i32 %add, %x
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
