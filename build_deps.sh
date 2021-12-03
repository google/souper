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

# hiredis latest as of May 7 2021
hiredis_commit=667dbf536524ba3f28c1d964793db1055c5a64f2
llvm_repo=https://github.com/regehr/llvm-project.git
# llvm_commit specifies the git branch or hash to checkout to
llvm_commit=disable-peepholes-llvm15-1
klee_repo=https://github.com/regehr/klee
klee_branch=klee-for-souper-13
alive_commit=v4
alive_repo=https://github.com/manasij7479/alive2.git
z3_repo=https://github.com/Z3Prover/z3.git
z3_commit=z3-4.11.2

llvm_build_type=Release
if [ -n "$1" ] ; then
  llvm_build_type="$1"
  shift
fi

z3_srcdir=$(pwd)/third_party/z3
z3_builddir=$(pwd)/third_party/z3-build
z3_installdir=$(pwd)/third_party/z3-install
(git clone $z3_repo $z3_srcdir && git -C $z3_srcdir checkout $z3_commit)
mkdir -p $z3_builddir
(cd $z3_builddir && cmake -Wno-dev ../z3 -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$z3_installdir -DZ3_BUILD_LIBZ3_SHARED=On -DZ3_BUILD_PYTHON_BINDINGS=Off && ninja && ninja install)

if [[ "$OSTYPE" == "darwin"* ]]; then
  # Mac
  Z3_SHAREDLIB=libz3.dylib
else
  Z3_SHAREDLIB=libz3.so
fi

alivedir=$(pwd)/third_party/alive2
alive_builddir=$(pwd)/third_party/alive2-build
mkdir -p $alivedir $alive_builddir
git clone $alive_repo $alivedir --branch $alive_commit

if [ -n "`which ninja`" ] ; then
  (cd $alive_builddir && cmake ../alive2 -DZ3_LIBRARIES=$z3_installdir/lib/$Z3_SHAREDLIB -DZ3_INCLUDE_DIR=$z3_installdir/include -DCMAKE_BUILD_TYPE=$llvm_build_type -GNinja)
  ninja -C $alive_builddir
else
  (cd $alive_builddir && cmake ../alive2 -DZ3_LIBRARIES=$z3_installdir/lib/$Z3_SHAREDLIB -DZ3_INCLUDE_DIR=$z3_installdir/include -DCMAKE_BUILD_TYPE=$llvm_build_type)
  make -C $alive_builddir -j $ncpus
fi

llvm_srcdir=$(pwd)/third_party/llvm-project
llvm_builddir=$(pwd)/third_party/llvm-${llvm_build_type}-build
llvm_installdir=$(pwd)/third_party/llvm-${llvm_build_type}-install

mkdir -p $llvm_srcdir
(cd $llvm_srcdir && git init && git remote add origin $llvm_repo && git fetch origin $llvm_commit && git reset --hard FETCH_HEAD)

mkdir -p $llvm_builddir

cmake_flags="-DCMAKE_INSTALL_PREFIX=$llvm_installdir -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_FORCE_ENABLE_STATS=ON -DCMAKE_BUILD_TYPE=$llvm_build_type -DLLVM_ENABLE_Z3_SOLVER=OFF -DLLVM_TARGETS_TO_BUILD=X86;AArch64 -DLLVM_ENABLE_PROJECTS=clang;compiler-rt"

if [ -n "`which ninja`" ] ; then
  (cd $llvm_builddir && cmake ${llvm_srcdir}/llvm -G Ninja $cmake_flags -DCMAKE_CXX_FLAGS="-DDISABLE_WRONG_OPTIMIZATIONS_DEFAULT_VALUE=true -DDISABLE_PEEPHOLES_DEFAULT_VALUE=false" "$@")
  ninja -C $llvm_builddir
  ninja -C $llvm_builddir install
else
  (cd $llvm_builddir && cmake $cmake_flags -DCMAKE_CXX_FLAGS="-DDISABLE_WRONG_OPTIMIZATIONS_DEFAULT_VALUE=true -DDISABLE_PEEPHOLES_DEFAULT_VALUE=false" "$@")
  make -C $llvm_builddir -j $ncpus
  make -C $llvm_builddir -j $ncpus install
fi

# we want these but they don't get installed by default
cp $llvm_builddir/bin/llvm-lit $llvm_installdir/bin
cp $llvm_builddir/bin/FileCheck $llvm_installdir/bin
cp $llvm_builddir/lib/libllvm_gtest_main.a $llvm_installdir/lib
cp $llvm_builddir/lib/libllvm_gtest.a $llvm_installdir/lib

kleedir=$(pwd)/third_party/klee

if [ -d third_party/klee/.git ] ; then
  (cd $kleedir && git fetch)
else
  git clone -b $klee_branch $klee_repo $kleedir
fi

hiredis_srcdir=$(pwd)/third_party/hiredis
hiredis_installdir=$(pwd)/third_party/hiredis-install

if [ -d $hiredis_srcdir/.git ] ; then
  (cd $hiredis_srcdir && git fetch)
else
  git clone https://github.com/redis/hiredis.git $hiredis_srcdir
fi

mkdir -p $hiredis_installdir/include/hiredis
mkdir -p $hiredis_installdir/lib

(cd $hiredis_srcdir && git checkout $hiredis_commit && make libhiredis.a &&
 cp -r alloc.h hiredis.h async.h read.h sds.h adapters ${hiredis_installdir}/include/hiredis &&
 cp libhiredis.a ${hiredis_installdir}/lib)
