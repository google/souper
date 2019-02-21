; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=bitreverse -S -o - %s | %FileCheck %s \
; RUN: --implicit-check-not="shl i8" --implicit-check-not="and i8" --implicit-check-not="or i8" --implicit-check-not="lshr i8"

; Below LLVM IR is retrieved by compiling following C program:
; char reverseBits(char num)
; {
;   char  NO_OF_BITS = sizeof(num) * 8;
;   char reverse_num = 0;
;   int i;
;   for (i = 0; i < NO_OF_BITS; i++)
;   {
;       if((num & (1 << i)))
;          reverse_num |= 1 << ((NO_OF_BITS - 1) - i);
;  }
;   return reverse_num;
; }
;
; `clang -O3 -c -emit-llvm bitrev.c`

; CHECK: define i8 @foo(i8 %num) {
define i8 @foo(i8 %num) {
entry:
  %0 = shl i8 %num, 7
  %and.1 = shl i8 %num, 5
  %1 = and i8 %and.1, 64
  %2 = or i8 %1, %0
  %and.2 = shl i8 %num, 3
  %3 = and i8 %and.2, 32
  %4 = or i8 %3, %2
  %and.3 = shl i8 %num, 1
  %5 = and i8 %and.3, 16
  %6 = or i8 %5, %4
  %and.4 = lshr i8 %num, 1
  %7 = and i8 %and.4, 8
  %8 = or i8 %6, %7
  %and.5 = lshr i8 %num, 3
  %9 = and i8 %and.5, 4
  %10 = or i8 %8, %9
  %and.6 = lshr i8 %num, 5
  %11 = and i8 %and.6, 2
  %12 = or i8 %10, %11
  %num.lobit = lshr i8 %num, 7
  %reverse_num.1.7 = or i8 %12, %num.lobit
  ; CHECK: call i8 @llvm.bitreverse.i8(i8 %num)
  ret i8 %reverse_num.1.7
}
