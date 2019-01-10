; REQUIRES: solver

; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=fshl,const -S -o - | %FileCheck %s --check-prefix=COSTMODEL
; RUN: %llvm-as %s -o - | %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-ignore-cost -souper-synthesis-comps=fshl,const -S -o - | %FileCheck %s --check-prefix=NOCOSTMODEL

define i32 @normal(i32 %a, i32 %b) {
; COSTMODEL: define i32 @normal(i32 %a, i32 %b) {
; COSTMODEL-NEXT: %highpart = shl i32 %a, 16
; COSTMODEL-NEXT: %lowpart = lshr i32 %b, 16
; COSTMODEL-NEXT: %conv2 = or i32 %highpart, %lowpart
; COSTMODEL-NEXT: ret i32 %conv2
; COSTMODEL-NEXT: }
;
; NOCOSTMODEL: define i32 @normal(i32 %a, i32 %b) {
; NOCOSTMODEL-NEXT: %1 = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 16)
; NOCOSTMODEL-NEXT: ret i32 %1
; NOCOSTMODEL-NEXT: }

  %highpart = shl i32 %a, 16
  %lowpart = lshr i32 %b, 16
  %conv2 = or i32 %highpart, %lowpart
  ret i32 %conv2
}
