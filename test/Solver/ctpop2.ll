; RUN: %llvm-as -o %t %s
; RUN: %souper -check -solver-timeout=60 -souper-only-infer-i1 %t

declare i20 @llvm.ctpop.i20(i20) nounwind readnone
declare void @sink(i1) nounwind readnone

define void @foo(i20 %x, i20 %y) {
entry:
  %pop1 = call i20 @llvm.ctpop.i20(i20 %x)
  %notx = xor i20 %x, -1
  %pop2 = call i20 @llvm.ctpop.i20(i20 %notx)
  %sum = add i20 %pop1, %pop2
  %cmp2 = icmp eq i20 %sum, 20, !expected !1
  call void @sink(i1 %cmp2)
  %shift = lshr exact i20 %x, %y
  %pop3 = call i20 @llvm.ctpop.i20(i20 %shift)
  %cmp3 = icmp eq i20 %pop1, %pop3, !expected !1
  call void @sink(i1 %cmp3)
  ret void
}

!1 = !{i1 1}
