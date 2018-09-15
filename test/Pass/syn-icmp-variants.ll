; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-ignore-cost -souper-synthesis-comps=ne,eq,ule,sle,ult,slt -S -o - %s | %FileCheck %s

define i1 @syn_eq(i32 %x, i32 %y) #0 {
  %a = icmp slt i32 %x, %y
  %b = xor i1 %a, true
  %c = icmp sgt i32 %x, %y
  %d = xor i1 %c, true
  %r = and i1 %b, %d
  ; CHECK: icmp eq i32
  ret i1 %r
}

define i1 @syn_ne(i32 %x, i32 %y) #0 {
  %a = icmp eq i32 %x, %y
  %b = xor i1 %a, true
  ; CHECK: icmp ne i32
  ret i1 %b
}

define i1 @syn_slt(i32 %x, i32 %y) #0 {
  %a = icmp sge i32 %x, %y
  %b = xor i1 %a, true
  ; CHECK: icmp slt i32 %x, %y
  ret i1 %b
}

define i1 @syn_sgt(i32 %x) #0 {
  %a = icmp sge i32 0, %x
  %b = xor i1 %a, true
  ; CHECK: icmp sgt i32 %x, 0
  ret i1 %b
}

define i1 @syn_sle(i32 %x, i32 %y) #0 {
  %a = icmp sgt i32 %x, %y
  %b = xor i1 %a, true
  ; CHECK: icmp sle i32 %x, %y
  ret i1 %b
}

define i1 @syn_sgt2(i32 %x) #0 {
  %a = icmp sgt i32 1, %x
  %b = xor i1 %a, true
  ; CHECK: icmp sgt i32 %x, 0
  ret i1 %b
}

define i1 @syn_ult(i32 %x, i32 %y) #0 {
  %a = icmp uge i32 %x, %y
  %b = xor i1 %a, true
  ; CHECK: icmp ult i32 %x, %y
  ret i1 %b
}

define i1 @syn_ule(i32 %x, i32 %y) #0 {
  %a = icmp ugt i32 %x, %y
  %b = xor i1 %a, true
  ; CHECK: icmp ule i32 %x, %y
  ret i1 %b
}

define i1 @syn_ugt(i32 %x) #0 {
  %a = icmp ugt i32 1, %x
  %b = xor i1 %a, true
  ; CHECK: icmp ugt i32 %x, 0
  ret i1 %b
}
