; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @foo(i32 %x) #0 {
entry:
  %mul = mul i32 %x, %x
  %cmp = icmp uge i32 %mul, %x
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
