; REQUIRES: solver

; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare i16 @llvm.bswap.i16(i16) #0

define i16 @foo(i16 %x) {
entry:
  %0 = shl i16 %x, 8
  %1 = lshr i16 %x, 8
  %2 = or i16 %0, %1
  %swap1 = call i16 @llvm.bswap.i16(i16 %x)
  %cmp1 = icmp eq i16 %swap1, %2, !expected !1
  ret i16 %swap1
}

!1 = !{i1 1}
