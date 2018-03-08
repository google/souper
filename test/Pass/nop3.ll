; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper %solver -souper-infer-nop -S -o - %s | %FileCheck %s

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define i32 @func(i32 %x) local_unnamed_addr #0 {
entry:
  %b = call i1 @branch(i32 %x)
  %call = call i32 @a(i32 %x)
  br i1 %b, label %bb1, label %bb2
  
bb1:
  %call_bb1 = add i32 0, %call
  br label %jmp

bb2:
  %call_bb2 = add i32 1, %call
  br label %jmp

jmp:
  %c = phi i32 [%call_bb1, %bb1], [%call_bb2, %bb2]
  %d = add i32 0, %c
  ; CHECK: ret i32 %c
  ret i32 %d
}

; Function Attrs: nounwind readnone
declare i32 @a(i32) local_unnamed_addr #1
declare i1 @branch(i32) local_unnamed_addr #1

