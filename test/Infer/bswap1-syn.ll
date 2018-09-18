; REQUIRES: solver, solver-model

; RUN: %llvm-as -o %t1 %s
; RUN: %souper %solver -souper-infer-inst -souper-synthesis-comps=const %t1 > %t2
; RUN: %FileCheck %s -check-prefix=SUCCESS < %t2

; SUCCESS: cand %0 873647531:i32

; Function Attrs: nounwind readnone
declare i32 @llvm.bswap.i32(i32) #0

define i32 @foo(i32 %x) {
entry:
  %swap = call i32 @llvm.bswap.i32(i32 2882343476) ;input is 0xABCD1234
  ret i32 %swap ;swapped result is 0x3412CDAB
}
