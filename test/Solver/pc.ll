; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

@a = common global i64 0, align 4

define void @test(i64 %x) {
entry:
  %cmp1 = icmp eq i64 %x, 2
  br i1 %cmp1, label %cont1, label %out
cont1:
  %cmp2 = icmp eq i64 %x, 2, !expected !1
  br i1 %cmp2, label %out, label %cont2
cont2:
  store i64 666, i64* @a, align 4
  br label %out
out:
  ret void
}

!1 = !{i1 1}
