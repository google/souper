; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper %solver -S -o - %s | %FileCheck %s

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define i32 @func() local_unnamed_addr #0 {
entry:
  %call = tail call i32 @abs(i32 -100) #2
  ; CHECK: ret i32 100
  ret i32 %call
}

; Function Attrs: nounwind readnone
declare i32 @abs(i32) local_unnamed_addr #1
