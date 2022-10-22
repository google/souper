#!/bin/bash
# Last argument is assumed to be a file with multiple inputs
# separated by empty lines

mkdir -p /tmp/scratch/
rm -f /tmp/scratch/*

infile=${@: -1} # Last argument
cmd=${*%${!#}} # All but the last argument

csplit --quiet --prefix=/tmp/scratch/opt --suffix-format=%02d.txt $infile '/^cand/ +1' '{*}'

for i in `ls -v /tmp/scratch/*`; do echo "echo \";$i \"&&" $cmd $i "&& echo";done > /tmp/cmdfile.txt
parallel --will-cite -k < /tmp/cmdfile.txt
