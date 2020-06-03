

; RUN: %llvm-as -o %t %s
; RUN: %souper -check %t

; Function Attrs: nounwind readnone
declare i8 @llvm.sadd.sat.i8(i8, i8)

define i8 @foo(i8 %x) {
entry:
  %add = call i8 @llvm.sadd.sat.i8(i8 -40, i8 -110), !expected !{ i8 -128 }
  ret i8 %add
}
