#!/bin/bash

#Example invocation:
#./test_infer.sh './build/souper-check -solver-timeout=60 --z3-path="/usr/bin/z3" -reinfer-rhs -souper-infer-inst -souper-exhaustive-synthesis' test/Solver/alive /tmp/results

mkdir -p $3
rm -r $3/*
for file in `find $2 -name "*.opt"`
do
  ( eval timeout 10m  $1 $file > $3/$(basename $file) 2>&1;
    if [ $? -eq 124 ]; then echo Timed Out >> $3/$(basename $file);fi;
    echo Completed $file;
  ) &
done
wait

success=0
failure=0
time_out=0
crash=0
for result in $3/*
do
  f=`grep 'Failed to infer RHS' $result | wc -l`
  s=`grep 'RHS inferred successfully' $result | wc -l`
  t=`grep 'timed out' $result | wc -l`
  c=`grep -E 'Crash|terminate|Error|Aborted' $result | wc -l`
  success=$(( $success + $s))
  failure=$(( $failure + $f))
  time_out=$(( $time_out + $t))
  crash=$(( $crash + $c))
done

echo Total Successes: $success
echo Total Failures: $failure
echo Total Timeouts: $time_out
echo Total Crashes: $crash
