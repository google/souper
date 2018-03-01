; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=trunc -S -o - %s | FileCheck %s

; Not working, since trunc can not be a component (L326-327 in lib/Infer/InstSynthesis.cpp)

define i1 @foo(i64 %x) {
entry:
  %a = and i64 %x, 1
  %cmp = icmp eq i64 %a, 0
  %b = select i1 %cmp, i1 0, i1 1
  ret i1 %b
}
