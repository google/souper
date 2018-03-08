; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check %t

define void @fn1() {
entry:
  %d1 = sdiv i32 1, 0, !expected !032
  %b1 = icmp eq i32 %d1, 0, !expected !0
  br i1 %b1, label %cond.true, label %cond.false

  %d2 = sdiv i32 0, 0, !expected !032
  %b2 = icmp eq i32 %d2, 0, !expected !0
  br i1 %b2, label %cond.true, label %cond.false

  %d3 = sdiv i32 0, undef, !expected !032
  %b3 = icmp eq i32 %d3, 0, !expected !0
  br i1 %b3, label %cond.true, label %cond.false

  %d4 = sdiv i32 undef, undef, !expected !032
  %b4 = icmp eq i32 %d4, 0, !expected !0
  br i1 %b4, label %cond.true, label %cond.false

  %d5 = udiv i32 1, 0, !expected !032
  %b5 = icmp eq i32 %d5, 0, !expected !0
  br i1 %b5, label %cond.true, label %cond.false

  %d6 = udiv i32 0, 0, !expected !032
  %b6 = icmp eq i32 %d6, 0, !expected !0
  br i1 %b6, label %cond.true, label %cond.false

  %d7 = udiv i32 0, undef, !expected !032
  %b7 = icmp eq i32 %d7, 0, !expected !0
  br i1 %b7, label %cond.true, label %cond.false

  %d8 = udiv i32 0, undef, !expected !032
  %b8 = icmp eq i32 %d8, 0, !expected !0
  br i1 %b8, label %cond.true, label %cond.false

  %d9 = urem i32 1, 0, !expected !032
  %b9 = icmp eq i32 %d9, 0, !expected !0
  br i1 %b9, label %cond.true, label %cond.false

  %d10 = urem i32 0, 0, !expected !032
  %b10 = icmp eq i32 %d10, 0, !expected !0
  br i1 %b10, label %cond.true, label %cond.false

  %d11 = urem i32 0, undef, !expected !032
  %b11 = icmp eq i32 %d11, 0, !expected !0
  br i1 %b11, label %cond.true, label %cond.false

  %d12 = urem i32 0, undef, !expected !032
  %b12 = icmp eq i32 %d12, 0, !expected !0
  br i1 %b12, label %cond.true, label %cond.false

  %d13 = srem i32 1, 0, !expected !032
  %b13 = icmp eq i32 %d13, 0, !expected !0
  br i1 %b13, label %cond.true, label %cond.false

  %d14 = srem i32 0, 0, !expected !032 
  %b14 = icmp eq i32 %d14, 0, !expected !0
  br i1 %b14, label %cond.true, label %cond.false

  %d15 = srem i32 0, undef, !expected !032
  %b15 = icmp eq i32 %d15, 0, !expected !0
  br i1 %b15, label %cond.true, label %cond.false

  %d16 = srem i32 0, undef, !expected !032
  %b16 = icmp eq i32 %d16, 0, !expected !0
  br i1 %b16, label %cond.true, label %cond.false

cond.true:
  br label %return

cond.false:
  br label %return

return:
  ret void
}

!0 = !{i1 0}
!032 = !{i32 0}
