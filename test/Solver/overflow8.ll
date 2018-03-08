; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32) #1

define i32 @foo(i32 %x) {
entry:
  %add = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %x, i32 1)
  %sum = extractvalue { i32, i1 } %add, 0
  %cmp = icmp uge i32 %sum, 0, !expected !1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
!1 = !{ i1 1 }
