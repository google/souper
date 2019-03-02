; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8)

define i1 @foo() {
entry:
  %sub = call { i8, i1 } @llvm.usub.with.overflow.i8(i8 0, i8 1)
  %bit = extractvalue { i8, i1 } %sub, 1, !expected !{ i1 1 }
  ret i1 %bit
}
