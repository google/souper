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

llvm_revision=218733
klee_commit=0c8c812db1d331f1e49e27ccd35f1288e58d97e6
hiredis_commit=8f60ee65327445ed8384290b4040685329eb03c5

llvm_build_type=Debug
if [ -n "$1" ] ; then
  llvm_build_type="$1"
  shift
fi

llvmdir=third_party/llvm
llvm_builddir=$llvmdir/$llvm_build_type

svn co -r $llvm_revision https://llvm.org/svn/llvm-project/llvm/trunk $llvmdir
svn co -r $llvm_revision https://llvm.org/svn/llvm-project/cfe/trunk $llvmdir/tools/clang
svn co -r $llvm_revision https://llvm.org/svn/llvm-project/compiler-rt/trunk $llvmdir/projects/compiler-rt

mkdir -p $llvm_builddir

cmake_flags=".. -DLLVM_TARGETS_TO_BUILD=host -DCMAKE_BUILD_TYPE=$llvm_build_type"

if [ -n "`which ninja`" ] ; then
  (cd $llvm_builddir && cmake -G Ninja $cmake_flags "$@")
  ninja -C $llvm_builddir
else
  (cd $llvm_builddir && cmake $cmake_flags "$@")
  make -C $llvm_builddir -j4
fi

kleedir=third_party/klee

if [ -d third_party/klee/.git ] ; then
  (cd $kleedir && git fetch)
else
  git clone https://github.com/klee/klee $kleedir
fi

(cd $kleedir && git checkout $klee_commit)

hiredisdir=third_party/hiredis

if [ -d $hiredisdir/.git ] ; then
  (cd $hiredisdir && git fetch)
else
  git clone https://github.com/redis/hiredis.git $hiredisdir
fi

mkdir -p $hiredisdir/install/include/hiredis
mkdir -p $hiredisdir/install/lib

(cd $hiredisdir && git checkout $hiredis_commit && make libhiredis.a &&
 cp -r hiredis.h async.h adapters install/include/hiredis &&
 cp libhiredis.a install/lib)
