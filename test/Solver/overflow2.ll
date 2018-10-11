; REQUIRES: solver

; RUN: %souper %solver -check %t

; Function Attrs: nounwind readnone
declare { i32, i1 } @llvm.smul.with.overflow.i32(i32, i32) #1

define i32 @foo(i32 %x) {
entry:
 %mul = call { i32, i1 } @llvm.smul.with.overflow.i32(i32 %x, i32 1000)
 %product = extractvalue { i32, i1 } %mul, 0
 %bit = extractvalue { i32, i1 } %mul, 1
 %conv = zext i1 %bit to i32
 ret i32 %conv
}
