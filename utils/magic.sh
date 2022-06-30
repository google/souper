#!/bin/bash
# Last argument is assumed to be a file with multiple inputs
# separated by empty lines

mkdir -p /tmp/scratch/
rm -f /tmp/scratch/*

infile=${@: -1}
cmd=${*%${!#}}

csplit --quiet --prefix=/tmp/scratch/opt --suffix-format=%02d.txt --suppress-matched $infile /^$/ {*}

for i in `ls -v /tmp/scratch/*`; do echo $cmd $i "&& echo";done > /tmp/cmdfile.txt
parallel --will-cite -k < /tmp/cmdfile.txt
