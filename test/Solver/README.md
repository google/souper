This directory is pretty much exclusively used for LLVM IR based tests for
checking whether souper understands equivalency of certain instructions,
without asking it to synthesize any new instructions.

That is done by placing `, !expected !1` on e.g. `icmp` LLVM IR instructions,
and adding `!1 = !{i1 1}` at the end of the file.

Example:
```llvm
; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define i32 @foo(i32 %x) {
entry:
  %add = add nsw i32 %x, 1
  %cmp = icmp sgt i32 %add, %x, !expected !1 ; <- `!1` is the unnamed metadata node to use
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

!1 = !{i1 1} ; <- `i1 1` means that we are asserting that `%cmp` should always evaluate `i1 true`
```
