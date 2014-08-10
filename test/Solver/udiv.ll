; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @mul(i32 %a, i32 %b) #0 {
entry:
  %div = udiv i32 %a, %b
  %cmp = icmp ult i32 %div, 0, !expected !0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!0 = metadata !{ i1 0 }
