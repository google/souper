; REQUIRES: solver

; RUN: %llvm-as -o %t %s
; RUN: %opt -load %pass -souper -dce %solver -souper-infer-inst -souper-synthesis-comps=and,or,ne,const,xor,add,sub,ashr -S -o - %s | %FileCheck %s

define i32 @alive0_0(i32, i32) local_unnamed_addr #0 {
  %Op0 = or i32 %0, %1
  %Op1 = xor i32 %0, %1
  %r = sub i32 %Op0, %Op1
  ret i32 %r

  ;CHECK-NOT: %Op0 = or i32 %0, %1
  ;CHECK-NOT: %Op1 = xor i32 %0, %1
  ;CHECK-NOT: %r = sub i32 %Op0, %Op1
  ;CHECK: %3 = and i32 {{(%0, %1)|(%1, %0)}}

}

define i32 @alive0_1(i16) local_unnamed_addr #0 {
  %c = icmp ult i16 -2, %0
  %X = sext i16 %0 to i32
  %r = select i1 %c, i32 %X, i32 -1
  ret i32 %r

  ;CHECK-NOT: %c = icmp ult i16 -2, %0
  ;CHECK-NOT: %X = sext i16 %0 to i32
  ;CHECK-NOT: %r = select i1 %c, i32 %X, i32 -1
  ;CHECK: ret i32 -1
}
