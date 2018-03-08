; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=bswap -S -o - %s | %FileCheck %s

define i64 @foo(i64 %x) {
entry:
  ;CHECK-NOT: %a = and i64 %x, 18374686479671623680
  %a = and i64 %x, 18374686479671623680 ; 0xFF00000000000000
  ;CHECK-NOT: %b = lshr i64 %a, 56
  %b = lshr i64 %a, 56
  ;CHECK-NOT: %c = and i64 %x, 71776119061217280
  %c = and i64 %x, 71776119061217280    ; 0x00FF000000000000
  ;CHECK-NOT:  %d = lshr i64 %c, 40
  %d = lshr i64 %c, 40
  ;CHECK-NOT: %e = and i64 %x, 280375465082880
  %e = and i64 %x, 280375465082880      ; 0x0000FF0000000000
  ;CHECK-NOT: %f = lshr i64 %e, 24
  %f = lshr i64 %e, 24
  ;CHECK-NOT: %g = and i64 %x, 1095216660480
  %g = and i64 %x, 1095216660480        ; 0x000000FF00000000
  ;CHECK-NOT: %h = lshr i64 %g, 8
  %h = lshr i64 %g, 8
  ;CHECK-NOT: %i = and i64 %x, 4278190080
  %i = and i64 %x, 4278190080           ; 0x00000000FF000000
  ;CHECK-NOT: %j = shl i64 %i, 8
  %j = shl i64 %i, 8
  ;CHECK-NOT: %k = and i64 %x, 16711680
  %k = and i64 %x, 16711680             ; 0x0000000000FF0000
  ;CHECK-NOT: %l = shl i64 %k, 24
  %l = shl i64 %k, 24
  ;CHECK-NOT: %m = and i64 %x, 65280
  %m = and i64 %x, 65280                ; 0x000000000000FF00
  ;CHECK-NOT: %n = shl i64 %m, 40
  %n = shl i64 %m, 40
  ;CHECK-NOT: %o = and i64 %x, 255
  %o = and i64 %x, 255                  ; 0x00000000000000FF
  ;CHECK-NOT: %p = shl i64 %o, 56
  %p = shl i64 %o, 56
  ;CHECK-NOT: %q = or i64 %b, %d
  %q = or i64 %b, %d
  ;CHECK-NOT: %r = or i64 %q, %f
  %r = or i64 %q, %f
  ;CHECK-NOT: %s = or i64 %r, %h
  %s = or i64 %r, %h
  ;CHECK-NOT: %t = or i64 %s, %j
  %t = or i64 %s, %j
  ;CHECK-NOT: %u = or i64 %t, %l
  %u = or i64 %t, %l
  ;CHECK-NOT: %v = or i64 %u, %n
  %v = or i64 %u, %n
  ;CHECK-NOT: %w = or i64 %v, %p
  %w = or i64 %v, %p
  ;CHECK: call i64 @llvm.bswap.i64(i64 %x)
  ret i64 %w
}
