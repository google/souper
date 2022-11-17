; RUN: %opt -load-pass-plugin %pass -passes='function(souper)'  -S -o - %s 2>&1 | %FileCheck %s

; CHECK-NOT: ptrtoint
; CHECK-NOT: trunc
; CHECK-NOT: shl
; CHECK: store

; ModuleID = 'foo.ll'
source_filename = "sqlite3.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define internal fastcc void @yy_reduce() unnamed_addr {
sw.bb115:
  %sub.ptr.rhs.cast139 = ptrtoint i8* undef to i64
  %sub.ptr.sub140 = sub i64 undef, %sub.ptr.rhs.cast139
  %conv141 = trunc i64 %sub.ptr.sub140 to i32
  %bf.value145 = and i32 %conv141, 2147483647
  %bf.shl146 = shl i32 %bf.value145, 1
  store i32 %bf.shl146, i32* undef, align 8
  ret void
}
