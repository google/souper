

; RUN: %llvm-as -o %t %s
; RUN: %souper -check %t

define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %sub = sub nsw i32 0, %a
  %sub1 = sub nsw i32 %sub, %b
  %cmp = icmp eq i32 %sub1, 0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
