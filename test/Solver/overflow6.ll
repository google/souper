; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.usub.with.overflow.i32(i32, i32) #1

define i32 @foo(i32 %x) {
entry:
  %cmp1 = icmp ugt i32 %x, 0
  %sub = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 0, i32 %x)
  %bit = extractvalue { i32, i1 } %sub, 1
  %ret = xor i1 %cmp1, %bit, !expected !0
  %conv = zext i1 %ret to i32
  ret i32 %conv
}
!0 = !{ i1 0 }
!1 = !{ i1 1 }
