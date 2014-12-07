; REQUIRES: solver

; RUN: llvm-as -o %t1 %s
; RUN: %souper %t1 > %t2
; RUN: %souper-check %solver -infer-rhs -print-replacement %t2 > %t3
; RUN: FileCheck %s -check-prefix=SUCCESS < %t3
; RUN: %parser-test -LHS < %t2
; RUN: %parser-test < %t3
; RUN: %souper-check %solver < %t3 | FileCheck -check-prefix=LGTM %s
; RUN: %souper-check %solver -infer-rhs %t2 > %t4
; RUN: cat %t2 %t4 | %souper-check %solver | FileCheck -check-prefix=LGTM %s

; SUCCESS: RHS inferred successfully
; LGTM: LGTM

declare i64 @llvm.bswap.i64(i64) #0

define void @foo(i64 %x) {
entry:
  %cmp = icmp eq i64 %x, %x
  ret void
}
