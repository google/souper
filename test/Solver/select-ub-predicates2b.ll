; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a, i1 %b) {
entry:
  %0 = add nsw i32 %a, 1
  br label %label1
foo1:
  %1 = add nsw i32 %a, 1
  br label %label1
label1:
  %2 = phi i32 [ %0, %entry ], [ %1, %foo1 ] 
  %3 = add nsw i32 %a, 1
  %4 = select i1 %b, i32 %2, i32 %3
  %res = icmp sgt i32 %4, %a, !expected !1
  ret i1 %res
}

!1 = !{ i1 1 }
