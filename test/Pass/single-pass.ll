

; RUN: %opt -load-pass-plugin %pass -passes='function(souper)'  -S -o - %s | %FileCheck %s

; CHECK-LABEL: @foo
define i32 @foo(i32 %x) {
entry:
  %add = add nsw i32 %x, 1
  %cmp = icmp sgt i32 %add, %x
  ; CHECK: ret i32 1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}

; CHECK-LABEL: @bar
define i32 @bar(i32 %x, i32 %y) {
  ; CHECK: icmp sgt
  %cmp = icmp sgt i32 %x, %y
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
