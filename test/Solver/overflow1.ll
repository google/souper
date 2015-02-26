; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32) #1

define i32 @foo(i32 %x) {
entry:
  %add = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %x, i32 1)
  %sum = extractvalue { i32, i1 } %add, 0
  %bit = extractvalue { i32, i1 } %add, 1
  %conv = zext i1 %bit to i32
  ret i32 %conv
}
