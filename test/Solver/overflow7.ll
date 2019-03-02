; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8)

define i1 @foo() {
entry:
  %sub = call { i8, i1 } @llvm.ssub.with.overflow.i8(i8 -10, i8 120)
  %bit = extractvalue { i8, i1 } %sub, 1, !expected !{ i1 1 }
  ret i1 %bit
}
