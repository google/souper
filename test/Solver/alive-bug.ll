; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i32 @mul(i32 %a, i32 %b) #0 {
entry:
  %0 = udiv i32 %a, %b
  %1 = icmp ne i32 0, %0
  %2 = icmp ne i32 1, %0
  %3 = and i1 %1, %2
  br i1 %3, label %true, label %false
true:
  %4 = icmp eq i32 0, %b, !expected !0
  %5 = zext i1 %4 to i32
  ret i32 %5
false:
  ret i32 555
}

!0 = !{i1 0}
