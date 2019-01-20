; REQUIRES: solver

; RUN: %opt -load %pass -souper %solver -S -o - %s -souper-debug-level=2 > %t 2>&1
; RUN: %FileCheck %s < %t

; Check that the souper pass dumps all replacements.

; CHECK: Listing all replacements for foo
; CHECK: 0:i1 = eq 1:i32, 1:i32
define i1 @foo() {
entry:
  %t = icmp eq i32 1, 1
  ret i1 %t
}
