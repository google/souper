; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %souper %solver -check -souper-infer-iN=false %t

define void @fn1() {
entry:
  %s11 = zext i8 undef to i16
  %s12 = sext i16 %s11 to i32
  %d1 = sdiv i32 1, %s12
  %b1 = icmp eq i32 %d1, 0, !expected !0
  br i1 %b1, label %cond.true, label %cond.false

  %s21 = sext i8 undef to i16
  %s22 = zext i16 %s21 to i32
  %d2 = sdiv i32 0, %s22
  %b2 = icmp eq i32 %d2, 0, !expected !0
  br i1 %b2, label %cond.true, label %cond.false

  %s31 = sext i8 0 to i16
  %s32 = zext i16 %s31 to i32
  %d3 = sdiv i32 0, %s32
  %b3 = icmp eq i32 %d3, 0, !expected !0
  br i1 %b3, label %cond.true, label %cond.false

  %s41 = zext i8 0 to i16
  %s42 = sext i16 %s41 to i32
  %d4 = sdiv i32 undef, %s42
  %b4 = icmp eq i32 %d4, 0, !expected !0
  br i1 %b4, label %cond.true, label %cond.false

  %s51 = zext i8 undef to i16
  %s52 = zext i16 %s51 to i32
  %d5 = udiv i32 1, %s52
  %b5 = icmp eq i32 %d5, 0, !expected !0
  br i1 %b5, label %cond.true, label %cond.false

  %s61 = zext i8 0 to i16
  %s62 = zext i16 %s61 to i32
  %d6 = udiv i32 0, %s62
  %b6 = icmp eq i32 %d6, 0, !expected !0
  br i1 %b6, label %cond.true, label %cond.false

  %s71 = zext i8 undef to i16
  %s72 = zext i16 %s71 to i32
  %d7 = udiv i32 0, %s72
  %b7 = icmp eq i32 %d7, 0, !expected !0
  br i1 %b7, label %cond.true, label %cond.false

  %s81 = zext i8 0 to i16
  %s82 = zext i16 %s81 to i32
  %d8 = udiv i32 0, %s82
  %b8 = icmp eq i32 %d8, 0, !expected !0
  br i1 %b8, label %cond.true, label %cond.false

  %ss1 = zext i1 0 to i8
  %ss2 = zext i8 %ss1 to i16
  %ss3 = sext i8 %ss1 to i16
  %ss4 = zext i16 %ss2 to i32
  %ss5 = sext i16 %ss2 to i32
  %ss6 = sext i16 %ss2 to i32
  %ss7 = zext i16 %ss2 to i32

  %d9 = urem i32 1, 0 
  %b9 = icmp eq i32 %d9, %ss4, !expected !0
  br i1 %b9, label %cond.true, label %cond.false

  %d10 = urem i32 0, 0 
  %b10 = icmp eq i32 %d10, %ss5, !expected !0
  br i1 %b10, label %cond.true, label %cond.false

  %d11 = urem i32 0, undef
  %b11 = icmp eq i32 %d11, %ss6, !expected !0
  br i1 %b11, label %cond.true, label %cond.false

  %d12 = urem i32 0, undef
  %b12 = icmp eq i32 %d12, %ss7, !expected !0
  br i1 %b12, label %cond.true, label %cond.false

  %d13 = srem i32 1, 0 
  %b13 = icmp eq i32 %d13, %ss7, !expected !0
  br i1 %b13, label %cond.true, label %cond.false

  %d14 = srem i32 0, 0 
  %b14 = icmp eq i32 %d14, %ss6, !expected !0
  br i1 %b14, label %cond.true, label %cond.false

  %d15 = srem i32 0, undef
  %b15 = icmp eq i32 %d15, %ss5, !expected !0
  br i1 %b15, label %cond.true, label %cond.false

  %d16 = srem i32 0, undef
  %b16 = icmp eq i32 %d16, %ss4, !expected !0
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

