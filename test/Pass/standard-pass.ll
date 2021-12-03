; RUN: %opt -load-pass-plugin %pass -O2 -S -o - %s -print-after-all 2>&1 | %FileCheck %s

; Check that peephole passes run at least 4 times

; CHECK: SouperPass on foo
; CHECK: SouperPass on foo
; CHECK: SouperPass on foo
; CHECK: SouperPass on foo
define void @foo() {
entry:
  ret void
}
