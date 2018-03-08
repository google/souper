; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i1 @foo(i32 %a, i32 %b, i32 %c) {
  switch i32 %a, label %9 [
    i32 0, label %1
    i32 1, label %2
    i32 2, label %3
  ]

; <label>:1                                       ; preds = %0
  br label %10

; <label>:2                                       ; preds = %0
  br label %10

; <label>:3                                       ; preds = %0
  %4 = shl i32 0, %c
  %5 = shl i32 0, %b
  %6 = mul nsw i32 8, %b
  %7 = mul nsw i32 %6, %c
  %8 = shl i32 0, %7
  %tmp1 = add i32 %4, %5
  %tmp2 = add i32 %tmp1, %8
  %tmp3 = add i32 %tmp2, %b
  %tmp4 = add i32 %tmp3, %c
  br label %10

; <label>:9                                       ; preds = %0
  br label %10

; <label>:10                                      ; preds = %9, %5, %3, %1
  %d.0 = phi i32 [ 10, %9 ], [ %tmp4, %3 ], [ 4, %2 ], [ 2, %1 ]
  %11 = icmp slt i32 %d.0, 32, !expected !1
  ret i1 %11
}

!1 = !{i1 1}
