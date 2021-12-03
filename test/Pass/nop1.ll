

; RUN: %opt -load-pass-plugin %pass -passes='function(souper)'  -S -o - %s 2>&1 | %FileCheck %s

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define i32 @func(i32 %x) local_unnamed_addr #0 {
entry:
  %call = call i32 @a(i32 %x)
; add dummy instruction w.r.t enumerative synthesis cost model
  %dumb = add i32 %call, 0
  br label %jmp

jmp:
  %c = phi i32 [%dumb, %entry]
  ; CHECK: ret i32 %call
  ret i32 %c
}

; Function Attrs: nounwind readnone
declare i32 @a(i32) local_unnamed_addr #1
