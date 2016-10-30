; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i32 @foo(i32 %x0) {
entry:
  %shr = ashr i32 %x0, 27
  %sub = add nsw i32 %shr, -27
  %tobool = icmp ne i32 %sub, 0, !expected !1
  %shr1 = lshr i32 1, %sub
  %cond = select i1 %tobool, i32 %sub, i32 %shr1
  %cmp = icmp ne i32 %cond, 0, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = !{ i1 1 }
