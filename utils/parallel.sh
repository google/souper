#!/bin/bash
# Last argument is assumed to be a dir with multiple inputs

indir=${@: -1} # Last argument
cmd=${*%${!#}} # All but the last argument

mkdir -p "${indir}r"
mkdir -p "${indir}d"

rm "${indir}r"/*
cp "${indir}"/* "${indir}r"

mkdir -p "${indir}t"
rm "${indir}t"/*

mkdir -p "${indir}d"
rm "${indir}d"/*

for i in `ls -v $indir/*`; do echo "timeout 300" $cmd $i " > " ${indir}t/`basename $i` " 2> " ${indir}d/`basename $i` " && cp " ${indir}t/`basename $i` ${indir}r/ ;done > /tmp/cmdfile.txt

# for i in `ls -v $indir/*`; do echo "timeout 300" $cmd $i " > " ${indir}r/`basename $i`;done > /tmp/cmdfile.txt

parallel --will-cite < /tmp/cmdfile.txt
