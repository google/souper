; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i8, i1 } @llvm.umul.with.overflow.i8(i8, i8)

define i1 @foo() {
entry:
 %mul = call { i8, i1 } @llvm.umul.with.overflow.i8(i8 16, i8 16)
 %bit = extractvalue { i8, i1 } %mul, 1, !expected !{ i1 1 }
 ret i1 %bit
}
