; REQUIRES: solver

; RUN: llvm-as -o %t %s
; RUN: %souper %solver -check %t

define void @fn1(i32 %a, i32 %b, i16 signext %c) #0 {
entry:
  %tobool = icmp ne i32 %a, 0
  %lor.ext = zext i1 %tobool to i32
  %div = sdiv i32 %lor.ext, -2
  %conv = trunc i32 %div to i16
  %tobool1 = icmp ne i16 %conv, 0, !expected !0
  br i1 %tobool1, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  ret void
}

!0 = metadata !{ i1 0 }
