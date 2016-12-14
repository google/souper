; REQUIRES: solver
; RUN: llvm-as %s -o - | %souper %solver -souper-infer-nop > %t2
; RUN: FileCheck %s < %t2
; CHECK: cand %5 %0

; ModuleID = 'foo.ll'
source_filename = "foo.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @nop1(i32) #0 {
  %2 = shl i32 %0, 16
  %3 = lshr i32 %0, 16
  %4 = lshr i32 %2, 16
  %5 = shl i32 %3, 16
  %6 = or i32 %4, %5
  ret i32 %6
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.9.0 (tags/RELEASE_390/final)"}
