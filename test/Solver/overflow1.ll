; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i8, i1 } @llvm.sadd.with.overflow.i8(i8, i8)

define i1 @foo() {
entry:
  %add = call { i8, i1 } @llvm.sadd.with.overflow.i8(i8 125, i8 5)
  %bit = extractvalue { i8, i1 } %add, 1, !expected !0
  ret i1 %bit
}
!0 = !{ i1 1 }
