; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i1 @foo(i32 %a, i32 %b, i32 %c) {
  switch i32 %a, label %9 [
    i32 0, label %1
    i32 1, label %3
    i32 2, label %5
  ]

; <label>:1                                       ; preds = %0
  %2 = shl i32 0, %c
  br label %11

; <label>:3                                       ; preds = %0
  %4 = shl i32 0, %b
  br label %11

; <label>:5                                       ; preds = %0
  %6 = mul nsw i32 8, %b
  %7 = mul nsw i32 %6, %c
  %8 = shl i32 0, %7
  br label %11

; <label>:9                                       ; preds = %0
  %10 = add nsw i32 %b, %c
  br label %11

; <label>:11                                      ; preds = %9, %5, %3, %1
  %d.0 = phi i32 [ %10, %9 ], [ %8, %5 ], [ %4, %3 ], [ %2, %1 ]
  %12 = icmp slt i32 %d.0, 32
  ret i1 %12
}
