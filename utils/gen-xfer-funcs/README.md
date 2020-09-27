# What this code is

This directory is even more experimental research software than the
rest of Souper, please don't expect it to work or be at all useful.

# Running this code

- go to `$BUILDDIR/utils/gen-xfer-funcs`

- run `./run_n.pl N` where N is the number of cores you want to use;
  this will create a bunch of directories such as work0, work1, each
  containing the work done by one core

- 

# TODO

- track precision of ones and zeroes separately

- lots of fun special cases for funnel shifts:
  a, b, c
  a, b, c
  a, a, b
  etc.

- don't count precision for the case where both inputs are
constants, since this doesn't happen in practice (but still check
these cases for soundness)

- will eventually need some sort of APInt optimizer

- have a script for harvesting a handful of the best functions

- evolve the zeroes and the ones separately, for known bits

- do some pruning of unsound ones on the fly?
  obbasionally test at higher width or else using a solver

- test mutation / generation / splicing in isolation
  or add instrumentation looking for which operators produce improved results

- do some tracking of whether improvements are typically incremental or all at once
  if incremental, may be safe to drop bad performers after a while

- during testing, don't drop the cases that always result in poison

- generate narrow constants? 0, 1, W-1, ...

- add infrastructure for comparing against precision of our hand-written functions

- pre-compute results up to 8 bits, to do better soundness and precision testing
  save these in files

- look closely at all hand-written functions and make sure we support all primitives they use

- reduce expressiveness until can nail the precise xor transfer function

- look at what mutate is actually doing, make sure it works

- add a separate set of available values and operations for booleans

- optionally emit smtlib to verify higher bitwidths

- support intersects, isSubsetOf, ctpop

- support getHiBits, getLoBits, getOneBitSet, getBitsSet, getBitsSetWithWrap, 

- support overflow and saturating ops?

- support arguments limited by bitwidth
  shl, ashr, lshr, rotl, rotr, [], setbit, clearbit, setHighBits
  could add a class of values between 0 and W-1?

- countMinLeadingZeroes and its friends

- support isSplat(x), isMask(x), isSignedIntN(x), isIntN(x)

- support byteswap somehow? not clear what to do in narrow cases

- reject already-seen candidates using a cache of hashes?

- revisit strategy for resynchronizing the stream

- std::min and max?

- enforce a max length on specs?

- get rid of a few things like ReverseBits?

- do swarm stuff
  flags
  cohort size
  kill after no improvement for 3 hours, or something
  probabilities in this script

- figure out how to use isConstant/getConstant

- support other domains
  bivalent
  required / don't care
  constantranges
  single bit analyses

- derive special case transfer functions for
  subset of args is constant
  pseudo-unary
  UB flags
  commonly seen collections of instructions
    use solver to identify cases where precision is being dropped

