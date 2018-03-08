; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define void @fn1() {
entry:
  %s1 = add i32 0, 0
  %d1 = sdiv i32 1, %s1
  %b1 = icmp eq i32 %d1, 0, !expected !0
  br i1 %b1, label %cond.true, label %cond.false

  %s2 = sub i32 0, 0
  %d2 = sdiv i32 1, %s2
  %b2 = icmp eq i32 %d2, 0, !expected !0
  br i1 %b2, label %cond.true, label %cond.false

  %s3 = sub i32 10, 10
  %d3 = sdiv i32 1, %s3 
  %b3 = icmp eq i32 %d3, 0, !expected !0
  br i1 %b3, label %cond.true, label %cond.false

  %s4 = mul i32 0, 10
  %d4 = sdiv i32 1, %s4
  %b4 = icmp eq i32 %d4, 0, !expected !0
  br i1 %b4, label %cond.true, label %cond.false

  %s5 = mul i32 10, 0
  %d5 = udiv i32 1, %s5
  %b5 = icmp eq i32 %d5, 0, !expected !0
  br i1 %b5, label %cond.true, label %cond.false

  %s6 = mul i32 0, undef
  %d6 = udiv i32 1, %s6
  %b6 = icmp eq i32 %d6, 0, !expected !0
  br i1 %b6, label %cond.true, label %cond.false

  %s7 = sub i32 undef, 0
  %d7 = udiv i32 1, %s7
  %b7 = icmp eq i32 %d7, 0, !expected !0
  br i1 %b7, label %cond.true, label %cond.false

  %s8 = shl i32 0, 1
  %d8 = udiv i32 1, %s8
  %b8 = icmp eq i32 %d8, 0, !expected !0
  br i1 %b8, label %cond.true, label %cond.false

  %s9 = sub i32 undef, undef
  %d9 = urem i32 1, %s9
  %b9 = icmp eq i32 %d9, 0, !expected !0
  br i1 %b9, label %cond.true, label %cond.false

  %s10 = mul i32 undef, 0
  %d10 = urem i32 1, %s10
  %b10 = icmp eq i32 %d10, 0, !expected !0
  br i1 %b10, label %cond.true, label %cond.false

  %s11 = udiv i32 0, 1
  %d11 = urem i32 1, %s11
  %b11 = icmp eq i32 %d11, 0, !expected !0
  br i1 %b11, label %cond.true, label %cond.false

  %s12 = sdiv i32 undef, 1
  %d12 = urem i32 1, %s12
  %b12 = icmp eq i32 %d12, 0, !expected !0
  br i1 %b12, label %cond.true, label %cond.false

  %s13 = srem i32 undef, 1
  %d13 = srem i32 1, %s13
  %b13 = icmp eq i32 %d13, 0, !expected !0
  br i1 %b13, label %cond.true, label %cond.false

  %s14 = urem i32 0, 1
  %d14 = srem i32 1, %s14
  %b14 = icmp eq i32 %d14, 0, !expected !0
  br i1 %b14, label %cond.true, label %cond.false

  %s15 = lshr i32 0, 1
  %d15 = srem i32 1, %s15
  %b15 = icmp eq i32 %d15, 0, !expected !0
  br i1 %b15, label %cond.true, label %cond.false

  %s16 = shl i32 0, 1
  %d16 = srem i32 1, %s16
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
!1 = !{i1 1}
