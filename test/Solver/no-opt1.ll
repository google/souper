; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @checked_add_2(i32 %a) #0 {
entry:
  %add = add i32 %a, 1
  %cmp = icmp sgt i32 %add, 0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
