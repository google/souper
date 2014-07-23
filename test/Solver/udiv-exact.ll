; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @fn1(i32 %a) #0 {
entry:
  %conv = zext i32 %a to i64
  %div = udiv exact i64 21474836490, %conv
  %tobool = icmp ne i64 %div, 0, !expected !1
  %land.ext = zext i1 %tobool to i32
  ret i32 %land.ext
}

!1 = metadata !{ i1 1 }
