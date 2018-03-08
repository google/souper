; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a) {
entry:
  %0 = add nsw i32 %a, 1
  br label %label1
label1:
  %1 = phi i32 [ %0, %entry ]
  %res = icmp sgt i32 %1, %a, !expected !1
  ret i1 %res
}

!1 = !{i1 1}
