; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare i64 @llvm.ctpop.i64(i64) nounwind readnone
declare i64 @llvm.ctlz.i64(i64) nounwind readnone
declare i64 @llvm.cttz.i64(i64) nounwind readnone
declare void @sink(i1) nounwind readnone

define void @foo(i64 %x) {
entry:
  %isnotzero = icmp ne i64 %x, 0
  %notx = xor i64 %x, -1
  %notxp1 = add i64 %notx, 1
  %and = and i64 %x, %notxp1
  %eq = icmp eq i64 %and, %x
  %power2 = and i1 %isnotzero, %eq
  %pop = call i64 @llvm.ctpop.i64(i64 %x)
  %lead = call i64 @llvm.ctlz.i64(i64 %x)
  %trail = call i64 @llvm.cttz.i64(i64 %x)
  %add = add i64 %lead, %trail
  br i1 %power2, label %ispower2, label %notpower2
ispower2:
  %cmp1 = icmp eq i64 %pop, 1, !expected !1
  %cmp2 = icmp eq i64 %add, 63, !expected !1
  call void @sink(i1 %cmp1)
  call void @sink(i1 %cmp2)
  ret void
notpower2:
  %cmp3 = icmp eq i64 %pop, 1, !expected !0
  %cmp4 = icmp eq i64 %add, 63, !expected !0
  call void @sink(i1 %cmp3)
  call void @sink(i1 %cmp4)
  ret void
}

!0 = !{i1 0}
!1 = !{i1 1}
