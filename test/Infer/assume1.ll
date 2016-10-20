; REQUIRES: solver, solver-model

; RUN: llvm-as -o %t1 %s
; RUN: %souper %solver -souper-infer-iN %t1 > %t2
; RUN: FileCheck %s < %t2

; CHECK-NOT: cand

; Function Attrs: nounwind
declare void @llvm.assume(i1)

define i32 @foo(i32 %x) {
entry:
  %and = and i32 %x, 1
  %tobool = icmp ne i32 %and, 0
  call void @llvm.assume(i1 %tobool)
  ret i32 %x
}
