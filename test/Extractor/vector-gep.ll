
; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

%class.x0 = type { i32*, i32* }
@x3 = global %class.x0 zeroinitializer, align 16

define i1 @foo() #0 {
entry:
  %0 = load <2 x i32*>, <2 x i32*>* bitcast (%class.x0* @x3 to <2 x i32*>*), align 16
  %1 = getelementptr i32, <2 x i32*> %0, <2 x i64> <i64 1, i64 1>
  store <2 x i32*> %1, <2 x i32*>* bitcast (%class.x0* @x3 to <2 x i32*>*), align 16
  %2 = ptrtoint <2 x i32*> %1 to <2 x i64>
  %3 = bitcast <2 x i64> %2 to i128
  %trunc = trunc i128 %3 to i64
  %res = icmp eq i64 %trunc, 0
  ret i1 %res
}

!0 = !{i1 0}
