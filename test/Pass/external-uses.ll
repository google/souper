; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=and,const,or,add,xor -S -o - %s | %FileCheck %s


define i32 @ia32_Blsr_unsupported_extra_use(i32, i32*) local_unnamed_addr #0 {
  %a = sub i32 0, %0
  %3 = or i32 %a, %0
  store i32 %3, i32* %1, align 4
  %4 = add i32 %3, %0
  ret i32 %4

  ;CHECK-NOT: and
}
