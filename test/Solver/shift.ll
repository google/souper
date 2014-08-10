; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define void @fn1(i32 %a) #0 {
entry:
  %shr = ashr exact i32 2, %a
  %cmp = icmp sgt i32 %shr, 2, !expected !0
  %conv = zext i1 %cmp to i32
  ret void
}

!0 = metadata !{ i1 0 }
