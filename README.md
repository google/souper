Souper is a superoptimizer for LLVM IR. It uses an SMT solver to help identify
missing peephole optimizations in LLVM's midend optimizers.

# Requirements

You will need a reasonably modern compiler toolchain. LLVM has instructions
on how to get one for Linux:
http://llvm.org/docs/GettingStarted.html#getting-a-modern-host-c-toolchain

You will also need CMake to build Souper and its dependencies.

To run Souper over a bitcode file, you will need an SMT solver. Souper
can use Z3, Boolector, CVC4, or STP. We recommend Z3.

If you have Go installed, you will also need the Redigo Redis client:
```
$ go get github.com/garyburd/redigo/redis
```

# Building Souper

1. Download and build dependencies:
```
$ ./update_deps.sh $buildtype $extra_cmake_flags
```
   $buildtype is optional; it defaults to Release and may be set to any LLVM
   build type.
   $extra_cmake_flags is optional. It is passed to CMake.

2. Run CMake from a build directory:
```
$ mkdir /path/to/souper-build
$ cd /path/to/souper-build
$ cmake -DCMAKE_BUILD_TYPE=$buildtype /path/to/souper
```
   Again, the build type is optional and defaults to Release. In any case it
   must match the build type used when compiling the dependencies. If you want
   to run Souper's full test suite, add this option to CMake (with the
   appropriate solver and path to the solver executable):
   -DTEST_SOLVER="-z3-path=/usr/bin/z3"

3. Run 'make' from the build directory.

4. Optionally run 'make check' to run Souper's test suite. To run the test suite
   under Valgrind, run 'make check LIT_ARGS="-v --vg --vg-leak"' instead. By
   default the solver is also run under Valgrind. This can be disabled by
   by adding --vg-arg=--trace-children-skip=/path/to/solver to LIT_ARGS.

Note that GCC 4.8 and earlier have a bug in handling multiline string
literals. You should build Souper using GCC 4.9+ or Clang.

# Using Souper

After following the above instructions, you will have a Souper
executable in /path/to/souper-build/souper and a Clang executable in
/path/to/souper/third_party/llvm/$buildtype/bin/clang.  You can use the
Clang executable to create an LLVM bitcode file like this:
```
$ /path/to/clang -emit-llvm -c -o /path/to/file.bc /path/to/file.c
```

Once you have a bitcode file you can invoke Souper with the path to the
bitcode file as a command line argument. You will also need one of these
flags to tell Souper where the SMT solver executable is:

```
-z3-path=<path>              - Path to Z3 executable
-boolector-path=<path>       - Path to Boolector executable
-cvc4-path=<path>            - Path to CVC4 executable
-stp-path=<path>             - Path to STP executable
```

For example:
```
$ /path/to/souper -z3-path=/usr/bin/stp /path/to/file.bc
```

Souper will extract SMT queries from the bitcode file and pass them to
a solver. Unsatisfiable queries (which represent missed optimization
opportunities) will cause Souper to print its internal representation
of the optimizable expression along with the shorter expression that
refines the original one.

Alternatively, you may immediately let Souper modify the bitcode and let
it apply the missed optimization opportunities by using the Souper llvm opt
pass. When loaded the pass will automatically register itself to run after
LLVM's regular peephole optimizations.

For example:
```
$ /path/to/clang -Xclang -load -Xclang /path/to/libsouperPass.so \
                 -mllvm -z3-path=/path/to/z3-build/z3 /path/to/file.c
```                 

Or to run the pass on its own:
```
$ /path/to/opt -load /path/to/libsouperPass.so -souper \
               -z3-path=/path/to/z3-build/z3 -o /path/to/file.opt.bc \
               /path/to/file.bc
```               

Or use the drop-in compiler replacements sclang and sclang++:
```
$ export SOUPER_SOLVER=-z3-path=/usr/bin/stp
$ /path/to/configure CC=/path/to/sclang CXX=/path/to/sclang++
$ make
```

Compilation using Souper can be sped up by caching queries. By default, Souper
uses a non-persistent RAM-based cache. The -souper-external-cache flag causes
Souper to cache its queries in a Redis database. For this to work, Redis >=
1.2.0 must be installed on the machine where you are running Souper and a Redis
server must be listening on the default port (6379).

sclang uses external caching by default since this often gives a substantial
speedup for large compilations. This behavior may be disabled by setting the
SOUPER_NO_EXTERNAL_CACHE environment variable. Souper's Redis cache does not yet
have any support for versioning; you should stop Redis and delete its dump file
any time Souper is upgraded.

Souper uses KLEE's Expr library to construct SMT queries in SMT-LIBv2 format
and pass them to a solver. KLEE's default encoding for symbolic inputs
is based on Theory of Arrays (QF_AUFBV). We experienced significant performance
boost in solvers by using pure BitVector encoding (QF_BV) instead. You can
use this workaround by pulling the following KLEE branch and recompiling
Souper:

```
$ cd /path/to/souper
$ rm -rf third_party/klee
$ git clone -b pure-bv-qf-llvm-5.0-patch https://github.com/rsas/klee third_party/klee
$ cd build
$ make
```

# Disclaimer

Please note that although some of the authors are employed by Google, this
is not an official Google product.
