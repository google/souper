; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t
; RUN: FileCheck %s < %t

; CHECK-NOT: cand

@x4 = common global i32 0, align 4
@x6 = common global i8 0, align 1
@x1 = common global i32 0, align 4
@x5 = common global i32 0, align 4
@x0 = common global i32 0, align 4
@x3 = common global i32 0, align 4
@x2 = common global i32 0, align 4

; Function Attrs: norecurse nounwind uwtable
define i32 @foo() #0 {
entry:
  %0 = load i32, i32* @x4, align 4
  %conv = trunc i32 %0 to i8
  store i8 %conv, i8* @x6, align 1
  %tobool = icmp eq i8 %conv, 0
  br i1 %tobool, label %if.else, label %if.then

if.then:                                          ; preds = %entry
  %1 = load i32, i32* @x1, align 4
  %cmp = icmp eq i32 %1, 0
  %conv1 = zext i1 %cmp to i32
  br i1 %cmp, label %cond.end, label %cond.false

cond.false:                                       ; preds = %if.then
  %rem = srem i32 %0, %1
  br label %cond.end

cond.end:                                         ; preds = %if.then, %cond.false
  %cond = phi i32 [ %rem, %cond.false ], [ %conv1, %if.then ]
  store i32 %cond, i32* @x5, align 4
  %tobool2 = icmp eq i32 %cond, 0
  br i1 %tobool2, label %if.end7, label %if.then3

if.then3:                                         ; preds = %cond.end
  store i32 0, i32* @x4, align 4
  br label %if.end7

if.else:                                          ; preds = %entry
  %2 = load i32, i32* @x0, align 4
  %tobool4 = icmp eq i32 %2, 0
  br i1 %tobool4, label %if.end6, label %if.then5

if.then5:                                         ; preds = %if.else
  store i32 9, i32* @x4, align 4
  br label %if.end6

if.end6:                                          ; preds = %if.else, %if.then5
  %3 = phi i32 [ %0, %if.else ], [ 9, %if.then5 ]
  %4 = load i32, i32* @x3, align 4
  %dec = add nsw i32 %4, -1
  store i32 %dec, i32* @x3, align 4
  br label %if.end7

if.end7:                                          ; preds = %cond.end, %if.then3, %if.end6
  %5 = phi i32 [ %0, %cond.end ], [ 0, %if.then3 ], [ %3, %if.end6 ]
  %cmp8 = icmp sgt i32 %5, 0
  %conv9 = zext i1 %cmp8 to i32
  store i32 %conv9, i32* @x2, align 4
  ret i32 %conv9
}
