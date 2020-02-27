#!/bin/bash -e

# Copyright 2014 The Souper Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

if [ -d "third_party" ]; then
  echo "Directory third_party exists, remove this directory before running build_deps.sh."
  exit 1;
fi

ncpus=$(command nproc 2>/dev/null || command sysctl -n hw.ncpu 2>/dev/null || echo 8)

# hiredis version 0.14.0
hiredis_commit=685030652cd98c5414ce554ff5b356dfe8437870
llvm_repo=https://github.com/llvm/llvm-project.git
# llvm_checkout specifies the git branch or hash to checkout to
llvm_checkout=b6b3fcdcb8cdfb887e26d27bee03b997d2d65888
klee_repo=https://github.com/rsas/klee
klee_branch=pure-bv-qf-llvm-7.0
alive_commit=9823174bb34fcb9c8e33c37e7e04d46bfe3a29a5
alive_repo=https://github.com/manasij7479/alive2.git
z3_repo=https://github.com/Z3Prover/z3.git
z3_commit=6330bf7d258c67cc086f4c2ee402e5d87af5d078

llvm_build_type=Release
if [ -n "$1" ] ; then
  llvm_build_type="$1"
  shift
fi

z3_srcdir=$(pwd)/third_party/z3
z3_installdir=$(pwd)/third_party/z3-install
git clone $z3_repo $z3_srcdir
mkdir -p $z3_installdir

(cd $z3_srcdir && git checkout $z3_commit && python scripts/mk_make.py --staticlib --prefix=$z3_installdir && cd build && make -j $ncpus install)

export PATH=$z3_installdir/bin:$PATH
export LD_LIBRARY_PATH=$z3_installdir/lib:$LD_LIBRARY_PATH

alivedir=third_party/alive2
alive_builddir=$alivedir/build
mkdir -p $alivedir $alive_builddir
git clone $alive_repo $alivedir/alive2
git -C $alivedir/alive2 checkout $alive_commit

if [ -n "`which ninja`" ] ; then
  (cd $alive_builddir && cmake ../alive2 -DZ3_LIBRARIES=$z3_installdir/lib/libz3.a -DZ3_INCLUDE_DIR=$z3_installdir/include -DCMAKE_BUILD_TYPE=$llvm_build_type -GNinja)
  ninja -C $alive_builddir
else
  (cd $alive_builddir && cmake ../alive2 -DZ3_LIBRARIES=$z3_installdir/lib/libz3.a -DZ3_INCLUDE_DIR=$z3_installdir/include -DCMAKE_BUILD_TYPE=$llvm_build_type)
  make -C $alive_builddir -j $ncpus
fi

llvm_srcdir=third_party/llvm
llvm_installdir=$(pwd)/${llvm_srcdir}/$llvm_build_type
llvm_builddir=$(pwd)/${llvm_srcdir}/${llvm_build_type}-build

git clone $llvm_repo $llvm_srcdir
git -C $llvm_srcdir checkout $llvm_checkout

# Apply instcombine switch patch
git -C ${llvm_srcdir} apply $(pwd)/patches/0001-enable-instcombine-switch.patch
# Disable the broken select -> logic optimizations
git -C ${llvm_srcdir} apply $(pwd)/patches/0002-disable-instcombine-select-to-logic.patch

mkdir -p $llvm_builddir

cmake_flags="../llvm -DCMAKE_INSTALL_PREFIX=$llvm_installdir -DLLVM_ENABLE_ASSERTIONS=On -DLLVM_FORCE_ENABLE_STATS=On -DCMAKE_BUILD_TYPE=$llvm_build_type -DZ3_INCLUDE_DIR=$z3_installdir/include -DZ3_LIBRARIES=$z3_installdir/lib/libz3.a -DLLVM_ENABLE_PROJECTS='llvm;clang;compiler-rt'"

if [ -n "`which ninja`" ] ; then
  (cd $llvm_builddir && cmake -G Ninja $cmake_flags "$@")
  ninja -C $llvm_builddir
  ninja -C $llvm_builddir install
else
  (cd $llvm_builddir && cmake $cmake_flags "$@")
  make -C $llvm_builddir -j $ncpus
  make -C $llvm_builddir -j $ncpus install
fi

# we want these but they don't get installed by default
cp $llvm_builddir/bin/llvm-lit $llvm_installdir/bin
cp $llvm_builddir/bin/FileCheck $llvm_installdir/bin
cp $llvm_builddir/lib/libgtest_main.a $llvm_installdir/lib
cp $llvm_builddir/lib/libgtest.a $llvm_installdir/lib

kleedir=third_party/klee

if [ -d third_party/klee/.git ] ; then
  (cd $kleedir && git fetch)
else
  git clone -b $klee_branch $klee_repo $kleedir
fi

hiredisdir=third_party/hiredis

if [ -d $hiredisdir/.git ] ; then
  (cd $hiredisdir && git fetch)
else
  git clone https://github.com/redis/hiredis.git $hiredisdir
fi

mkdir -p $hiredisdir/install/include/hiredis
mkdir -p $hiredisdir/install/lib

(cd $hiredisdir && git checkout $hiredis_commit && make libhiredis.a &&
 cp -r hiredis.h async.h read.h sds.h adapters install/include/hiredis &&
 cp libhiredis.a install/lib)
