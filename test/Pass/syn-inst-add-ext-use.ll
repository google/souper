; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper %solver -souper-infer-inst -souper-synthesis-comps=add,const -S -o - %s | %FileCheck %s

; Investigation and improvement are needed. Current instruction synthesis can
; not optimization instruction with outside uses smartly. This case shows this
; issue: %a, %b and %c all have external uses, and they should not be swiped
; out, thus synthesis a new instruction "add %x, 4" only for replacing %d is not
; necessary. Now souper does this redundant optimization.
; This issue is now provisionally fixed by considering the cost of instructions
; in the LHS with external uses to be 0.

@amem = common global i32 0, align 4
@bmem = common global i32 0, align 4
@cmem = common global i32 0, align 4

define i32 @foo(i32 %x) {
entry:
  %a = add i32 %x, 1
  store i32 %a, i32* @amem, align 4
  %b = add i32 %a, 1
  store i32 %b, i32* @bmem, align 4
  %c = add i32 %b, 1
  store i32 %c, i32* @cmem, align 4
  ;CHECK-NOT: add i32 %x, 4
  %d = add i32 %c, 1
  ret i32 %d
}
