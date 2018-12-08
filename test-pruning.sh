#!/bin/bash
# $1 - souper invocation command
# Example :
# ../test-pruning.sh './souper-check --z3-path="/usr/bin/z3" -reinfer-rhs -souper-exhaustive-synthesis -souper-exhaustive-synthesis-num-instructions=2  ~/tmp.opt'
# Make a prune function always return false to see this script being effective

out_prune=`mktemp`
out_noprune=`mktemp`
out_prune_filtered=`mktemp`
eval $1 -souper-dataflow-pruning=true -souper-dataflow-pruning-stats-level=1 > $out_prune
eval $1 -souper-dataflow-pruning=false > $out_noprune

tail -n+2 $out_prune > $out_prune_filtered

head -n1 $out_prune

cmp --silent $out_noprune $out_prune_filtered \
  || (echo "Different results" && cat $out_noprune $out_prune_filtered)

rm $out_noprune $out_prune $out_prune_filtered
