

; RUN: %llvm-as -o %t %s
; RUN: %souper -check %t

; Function Attrs: nounwind readnone
declare i8 @llvm.ssub.sat.i8(i8, i8)

define i8 @foo(i8 %x) {
entry:
  %add = call i8 @llvm.ssub.sat.i8(i8 100, i8 -120), !expected !{ i8 127 }
  ret i8 %add
}
