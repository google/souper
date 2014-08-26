; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

; XFAIL: *

define i32 @foo(i32 %x, i32 %y) {
entry:
  %add = add i32 %x, undef
  %cmp = icmp eq i32 %add, %y, !expected !1
  %ret = zext i1 %cmp to i32
  ret i32 %ret
}

!1 = metadata !{ i1 1 }
