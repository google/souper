; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t
; This test case input in hex is 0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

declare i256 @llvm.ctpop.i256(i256) nounwind readnone

define i1 @foo(i256 %x) {
entry:
  %pop = call i256 @llvm.ctpop.i256(i256 77194726158210796949047323339125271902179989777093709359638389338608753093290)
  %cmp = icmp eq i256 %pop, 128, !expected !1
  ret i1 %cmp
}

!1 = !{i1 1}
