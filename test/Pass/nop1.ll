; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: opt -load %pass -souper %solver -souper-infer-nop -S -o - %s | FileCheck %s

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define i32 @func(i32 %x) local_unnamed_addr #0 {
entry:
  %call = call i32 @a(i32 %x)
  br label %jmp

jmp:
  %c = phi i32 [%call, %entry]
  %d = add i32 0, %c
  ; CHECK: ret i32 %call
  ret i32 %d
}

; Function Attrs: nounwind readnone
declare i32 @a(i32) local_unnamed_addr #1
