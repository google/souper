; REQUIRES: solver

; RUN: %opt -load %pass -souper %solver -S -o - %s

@a = dso_local local_unnamed_addr global i32 0, align 4

define void @b() {
for.end:
  ret void

for.cond1:
  %0 = load i32, i32* @a
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* @a
  br label %for.cond1
}

