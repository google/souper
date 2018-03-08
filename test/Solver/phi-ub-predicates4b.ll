; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i1 @foo(i32 %a, i32 %b) {
entry:
  %0 = shl i32 0, %b
  br label %label1
foo1:
  %1 = shl i32 0, %a
  br label %label1
foo2:
  %2 = add i32 %0, %1
  br label %label1
foo3:
  %3 = mul nsw i32 8, %a
  %4 = mul nsw i32 %3, %b
  %5 = shl i32 0, %4
  br label %label1
foo4:
  %6 = add nsw i32 %a, %b
  %tmp1 = add i32 %6, %2
  %tmp2 = add i32 %tmp1, %5
  br label %label1
label1:
  %phi1 = phi i32 [ %0, %entry ], [ %1, %foo1 ], [ %2, %foo2 ], [ %5, %foo3 ], [ %tmp2, %foo4 ]
  br label %label2
foo5:
  br label %label2
foo6:
  br label %label2
label2:
  %phi2 = phi i32 [ 6, %foo5 ], [ %phi1, %label1 ], [ 2, %foo6 ] 
  %cmp = icmp slt i32 %phi2, 32, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
