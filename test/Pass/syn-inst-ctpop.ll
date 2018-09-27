; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-ignore-cost -souper-synthesis-comps=ctpop -S -o - %s | %FileCheck %s

; Translated from test/Infer/popcount6-syn.opt

define i32 @foo(i32 %x) {
entry:
  ;CHECK-NOT: %a = and i32 %x, 1431655765
  %a = and i32 %x, 1431655765
  ;CHECK-NOT: %b = lshr i32 %x, 1
  %b = lshr i32 %x, 1
  ;CHECK-NOT: %c = and i32 %b, 1431655765
  %c = and i32 %b, 1431655765
  ;CHECK-NOT: %d = add i32 %a, %c
  %d = add i32 %a, %c
  ;CHECK-NOT: %e = and i32 %d, 858993459
  %e = and i32 %d, 858993459
  ;CHECK-NOT: %f = lshr i32 %d, 2
  %f = lshr i32 %d, 2
  ;CHECK-NOT: %g = and i32 %f, 858993459
  %g = and i32 %f, 858993459
  ;CHECK-NOT: %h = add i32 %e, %g
  %h = add i32 %e, %g
  ;CHECK-NOT: %i = and i32 %h, 252645135
  %i = and i32 %h, 252645135
  ;CHECK-NOT: %j = lshr i32 %h, 4
  %j = lshr i32 %h, 4
  ;CHECK-NOT: %k = and i32 %j, 252645135
  %k = and i32 %j, 252645135
  ;CHECK-NOT: %l = add i32 %i, %k
  %l = add i32 %i, %k
  ;CHECK-NOT: %m = and i32 %l, 16711935
  %m = and i32 %l, 16711935
  ;CHECK-NOT: %n = lshr i32 %l, 8
  %n = lshr i32 %l, 8
  ;CHECK-NOT: %o = and i32 %n, 16711935
  %o = and i32 %n, 16711935
  ;CHECK-NOT: %p = add i32 %m, %o
  %p = add i32 %m, %o
  ;CHECK-NOT: %q = and i32 %p, 65535
  %q = and i32 %p, 65535
  ;CHECK-NOT: %r = lshr i32 %p, 16
  %r = lshr i32 %p, 16
  ;CHECK-NOT: %s = and i32 %r, 65535
  %s = and i32 %r, 65535
  ;CHECK-NOT: %t = add i32 %q, %s
  %t = add i32 %q, %s
  ;CHECK: call i32 @llvm.ctpop.i32(i32 %x)
  ret i32 %t
}
