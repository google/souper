

; RUN: %llvm-as -o %t %s
; RUN: %souper -check -souper-only-infer-iN %t

define i1 @foo(i32 %a) {
entry:
  %0 = add nsw i32 %a, 1
  br label %label1
foo1:
  %1 = add i32 %a, 1
  br label %label1
label1:
  %2 = phi i32 [ %0, %entry ], [ %1, %foo1 ]
  %res = icmp sgt i32 %2, %a
  ret i1 %res
}
