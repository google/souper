; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

declare i64 @llvm.cttz.i64(i64) nounwind readnone

define i64 @foo(i64 %x) {
entry:
  %count = call i64 @llvm.cttz.i64(i64 9223372036854775808) ;input is 0x8000000000000000
  %cmp = icmp eq i64 %count, 63, !expected !1
  %conv = zext i1 %cmp to i64
  ret i64 %conv
}

!1 = !{i1 1}
