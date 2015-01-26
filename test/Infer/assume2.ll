; REQUIRES: solver, solver-model

; RUN: llvm-as -o %t1 %s
; RUN: %souper %solver %t1 > %t2
; RUN: FileCheck -check-prefix=SUCCESS %s < %t2

; SUCCESS: cand %9 1:i1

; Function Attrs: nounwind
declare void @llvm.assume(i1)

define i1 @foo(i32 %x, i32 %y) {
entry:
  %and.x = and i32 %x, 1
  %tobool.x = icmp ne i32 %and.x, 0
  call void @llvm.assume(i1 %tobool.x)
  %and.y = and i32 %y, 1
  %tobool.y = icmp ne i32 %and.y, 0
  call void @llvm.assume(i1 %tobool.y)
  %add = add nsw i32 %x, %y
  %and = and i32 %add, 1
  %xor = xor i32 %and, 1
  %tobool = icmp ne i32 %xor, 0
  ret i1 %tobool
}
