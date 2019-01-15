; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

; Function Attrs: nounwind readnone
declare i64 @llvm.bswap.i64(i64) #0

define i1 @foo(i64 %x) {
entry:
  %swap = call i64 @llvm.bswap.i64(i64 12379570966709668117) ;input is 0xABCD123456780915
  %cmp = icmp eq i64 %swap, 1515875061223050667, !expected !1 ;swapped result is 0x150978563412CDAB
  ret i1 %cmp
}

!1 = !{i1 1}
