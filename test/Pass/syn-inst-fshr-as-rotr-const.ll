

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce -souper-infer-inst -souper-synthesis-comps=fshr,const -S -o - | %FileCheck %s --check-prefix=COSTMODEL
; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce -souper-infer-inst -souper-synthesis-ignore-cost -souper-synthesis-comps=fshr,const -S -o - | %FileCheck %s --check-prefix=NOCOSTMODEL

define i32 @rotate(i32 %x) {
; COSTMODEL: define i32 @rotate(i32 %x) {
; COSTMODEL-NEXT: %highpart = shl i32 %x, 16
; COSTMODEL-NEXT: %lowpart = lshr i32 %x, 16
; COSTMODEL-NEXT: %conv2 = or i32 %highpart, %lowpart
; COSTMODEL-NEXT: ret i32 %conv2
; COSTMODEL-NEXT: }
;
; NOCOSTMODEL: define i32 @rotate(i32 %x) {
; NOCOSTMODEL-NEXT: %1 = call i32 @llvm.fshr.i32(i32 %x, i32 %x, i32 16)
; NOCOSTMODEL-NEXT: ret i32 %1
; NOCOSTMODEL-NEXT: }

  %highpart = shl i32 %x, 16
  %lowpart = lshr i32 %x, 16
  %conv2 = or i32 %highpart, %lowpart
  ret i32 %conv2
}
