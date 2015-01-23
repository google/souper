; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

declare i64 @llvm.ctlz.i64(i64) nounwind readnone

define i64 @foo(i64 %x) {
entry:
  %count = call i64 @llvm.ctlz.i64(i64 281479271743489) ;input is 0x0001000100010001
  %cmp = icmp eq i64 %count, 15, !expected !1
  ret i64 %count
}

!1 = !{i1 1}
