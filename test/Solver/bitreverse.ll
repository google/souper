; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare i16 @llvm.bitreverse.i16(i16) #0
declare i16 @llvm.ctpop.i16(i16) #1

define i1 @foo(i16 %x) {
entry:
  %count1 = call i16 @llvm.ctpop.i16(i16 %x)
  %rev = call i16 @llvm.bitreverse.i16(i16 %x)
  %count2 = call i16 @llvm.ctpop.i16(i16 %rev)
  %cmp1 = icmp eq i16 %count1, %count2, !expected !1
  ret i1 %cmp1
}

!1 = !{i1 1}
