; RUN: %llvm-as -o %t %s
; RUN: %souper -check %t

define i16 @foo(i16 %x) #0 {
entry:
  %mul = mul nsw i16 %x, %x
  %cmp = icmp uge i16 %mul, %x
  %conv = zext i1 %cmp to i16
  ret i16 %conv
}
