; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @foo(i32 %x) #0 {
entry:
  %add = add i32 %x, 1
  %cmp = icmp ne i32 %add, 0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
