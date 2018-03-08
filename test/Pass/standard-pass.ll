; REQUIRES: solver

; RUN: %opt -load %pass -O2 %solver -S -o - %s -print-after-all 2>&1 | %FileCheck %s

; Check that peephole passes are registered at least twice.

; CHECK: IR Dump After Souper super-optimizer pass
; CHECK: IR Dump After Souper super-optimizer pass
define void @foo() {
entry:
  ret void
}
