#!/usr/bin/python3

import os
import subprocess
import sys
import time
import re
import itertools

num_instrs = [1, 2, 3]
files = ['enum0.opt', 'enum1.opt']
dataflows = ['false']

configs = itertools.product(num_instrs, files, dataflows)
# 0 - num-instrs, 1 - filename, 2 - dataflow

for conf in configs:
    cmd_enum = "./souper-check -z3-path=\"/usr/bin/z3\" -souper-enumerative-synthesis-debug-level=2 -solver-timeout=60 -infer-rhs -souper-enumerative-synthesis -souper-enumerative-synthesis-num-instructions=%d -souper-dataflow-pruning=%s -souper-enumerative-synthesis-skip-solver %s" % (conf[0], conf[2], conf[1])
    time_start = time.time()
    output = subprocess.getoutput(cmd_enum)
    time_end = time.time()
    guesses = re.search('There are.*Guesses', output).group()
    dataflowpruned = re.search('Dataflow Pruned .*', output).group()
    print('For %s(%d) (dataflow=%s): (took %f seconds). [%s | %s]' % (conf[1], conf[0], conf[2], round(time_end - time_start, 2), guesses, dataflowpruned))
