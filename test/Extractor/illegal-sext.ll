
; RUN: llvm-as -o %t %s
; RUN: %souper %t | %parser-test -LHS

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i1 @foo(i8** %a, i8* %b) #0 {
entry:
  %0 = load i8*, i8** %a, align 8
  %1 = getelementptr inbounds i8, i8* %0, i64 1
  %res = icmp ugt i8* %1, %b
  ret i1 %res
}

!0 = !{i1 0}
