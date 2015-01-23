; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare i32 @llvm.bswap.i32(i32) #0

define i32 @foo(i32 %x) {
entry:
  %swap = call i32 @llvm.bswap.i32(i32 2882343476) ;input is 0xABCD1234
  %cmp = icmp eq i32 %swap, 873647531, !expected !1 ;swapped result is 0x3412CDAB
  ret i32 %swap
}

!1 = !{i1 1}
