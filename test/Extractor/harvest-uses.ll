; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper %solver -souper-harvest-uses -S -o - %s | %FileCheck %s

define i1 @fn1(i1) {
  %x = and i1 1, %0
  br i1 %x, label %l1, label %l2

l1:
  ;CHECK: ret i1 true
  ret i1 %x

l2:
  ;CHECK: ret i1 false
  ret i1 %x
}

define i8 @fn2(i8) {
  %x = icmp slt i8 0, %0
  %y = sdiv i8 %0, 16
  ; CHECK-NOT: ashr i8 %0, 4
  br i1 %x, label %l1, label %l2
l1:
  ;CHECK: ret i8 %y
  ret i8 %y

l2:
  ;CHECK: ret i8 %y
  ret i8 %y
}

; TODO: support harvesting from not only instructions, but values
define i1 @fn3(i1) {
  br i1 %0, label %l1, label %l2

l1:
  ret i1 %0

l2:
  ret i1 %0
}
