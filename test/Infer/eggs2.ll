; REQUIRES: solver, solver-model

; RUN: %llvm-as -o %t1 %s
; RUN: %souper %solver -souper-infer-iN %t1 > %t2
; RUN: %FileCheck %s -check-prefix=SUCCESS < %t2

; SUCCESS: cand %12 301:i10

; A woman was carrying a large basket of eggs when a passer-by bumped her and
; she dropped the basket and all the eggs broke. The passer-by asked how many
; eggs there had been. The woman replied: "I don't remember exactly, but I do
; recall that whether I took out the eggs in batches of 2, 3, 4, 5, or 6 there
; was always one egg left over. When I took the eggs out in groups of seven, I
; emptied the basket."

define i10 @foo(i10 %x) {
entry:
  %rem1 = urem i10 %x, 2
  %cmp1 = icmp eq i10 %rem1, 1
  br i1 %cmp1, label %cont1, label %out
cont1:
  %rem2 = urem i10 %x, 3
  %cmp2 = icmp eq i10 %rem2, 1
  br i1 %cmp2, label %cont2, label %out
cont2:
  %rem3 = urem i10 %x, 4
  %cmp3 = icmp eq i10 %rem3, 1
  br i1 %cmp3, label %cont3, label %out
cont3:
  %rem4 = urem i10 %x, 5
  %cmp4 = icmp eq i10 %rem4, 1
  br i1 %cmp4, label %cont4, label %out
cont4:
  %rem6 = urem i10 %x, 7
  %cmp6 = icmp eq i10 %rem6, 0
  br i1 %cmp6, label %cont6, label %out
cont6:
  %cmp7 = icmp ult i10 %x, 500
  br i1 %cmp7, label %cont7, label %out
cont7:
  %res = add i10 %x, 0
  ret i10 %res
out:
  ret i10 0
}
