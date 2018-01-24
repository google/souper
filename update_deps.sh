#!/bin/sh -e

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

# hiredis version 0.13.3
hiredis_commit=010756025e8cefd1bc66c6d4ed3b1648ef6f1f95
llvm_branch=branches/release_60
klee_repo=https://github.com/rsas/klee
klee_branch=pure-bv-qf-llvm-6.0-patch

llvm_build_type=Release
if [ -n "$1" ] ; then
  llvm_build_type="$1"
  shift
fi

llvmdir=third_party/llvm
llvm_installdir=$(pwd)/$llvmdir/$llvm_build_type
llvm_builddir=$(pwd)/$llvmdir/${llvm_build_type}-build

svn co https://llvm.org/svn/llvm-project/llvm/${llvm_branch} $llvmdir
svn co https://llvm.org/svn/llvm-project/cfe/${llvm_branch} $llvmdir/tools/clang
svn co https://llvm.org/svn/llvm-project/compiler-rt/${llvm_branch} $llvmdir/projects/compiler-rt
mkdir -p $llvm_builddir

cmake_flags=".. -DCMAKE_INSTALL_PREFIX=$llvm_installdir -DLLVM_ENABLE_ASSERTIONS=On -DLLVM_TARGETS_TO_BUILD=host -DCMAKE_BUILD_TYPE=$llvm_build_type -DCMAKE_CXX_FLAGS=-DLLVM_ENABLE_STATS=true"

if [ -n "`which ninja`" ] ; then
  (cd $llvm_builddir && cmake -G Ninja $cmake_flags "$@")
  ninja -C $llvm_builddir
  ninja -C $llvm_builddir install
else
  (cd $llvm_builddir && cmake $cmake_flags "$@")
  make -C $llvm_builddir -j4
  make -C $llvm_builddir -j4 install
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
