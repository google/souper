; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define i1 @foo(i32 %a, i32 %b) {
entry:
  %0 = shl i32 %a, %b
  br label %label1
foo:
  %1 = shl i32 %b, %a
  br label %label1
foo2:
  %2 = mul nsw i32 8, %a
  %3 = mul nsw i32 %2, %b
  %4 = shl i32 0, %3
  br label %label1
label1:
  %phi1 = phi i32 [ %0, %entry ], [ %1, %foo ], [ %4, %foo2 ] 
  br label %label2
foo3:
  br label %label2
foo4:
  br label %label2
label2:
  %phi2 = phi i32 [ %phi1, %label1 ], [ 1, %foo3 ], [ %phi1, %foo4 ] 
  %phi3 = phi i32 [ %phi1, %label1 ], [ %phi2, %foo3 ], [ %phi2, %foo4 ] 
  %phi4 = phi i32 [ %phi1, %label1 ], [ %phi1, %foo3 ], [ 2, %foo4 ] 
  %cmp = icmp slt i32 %phi4, 32
  ret i1 %cmp
}
