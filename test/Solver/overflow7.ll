; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32) #1

define i32 @foo(i32 %x) {
entry:
  %cmp1 = icmp ugt i32 %x, 0
  %sub = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 0, i32 %x)
  %bit = extractvalue { i32, i1 } %sub, 1
  %ret = xor i1 %cmp1, %bit
  %conv = zext i1 %ret to i32
  ret i32 %conv
}
