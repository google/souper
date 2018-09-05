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

if [ -d "third_party" ]; then
  echo "Directory third_party exists, remove this directory before running build_deps.sh."
  exit 1;
fi

# hiredis version 0.13.3
hiredis_commit=010756025e8cefd1bc66c6d4ed3b1648ef6f1f95
llvm_branch=tags/RELEASE_700/final
klee_repo=https://github.com/rsas/klee
klee_branch=pure-bv-qf-llvm-7.0

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
# Disable the broken select -> logic optimizations
cat <<EOF | patch $llvmdir/lib/Transforms/InstCombine/InstCombineSelect.cpp
--- lib/Transforms/InstCombine/InstCombineSelect.cpp    (revision 326856)
+++ lib/Transforms/InstCombine/InstCombineSelect.cpp    (working copy)
@@ -1334,7 +1334,7 @@
     Worklist.Add(Cond);
     return &SI;
   }
-
+#if 0
   if (SelType->isIntOrIntVectorTy(1) &&
       TrueVal->getType() == CondVal->getType()) {
     if (match(TrueVal, m_One())) {
@@ -1370,7 +1370,6 @@
     if (match(FalseVal, m_Not(m_Specific(CondVal))))
       return BinaryOperator::CreateOr(TrueVal, FalseVal);
   }
-
   // Selecting between two integer or vector splat integer constants?
   //
   // Note that we don't handle a scalar select of vectors:
@@ -1378,7 +1377,8 @@
   // because that may need 3 instructions to splat the condition value:
   // extend, insertelement, shufflevector.
   if (SelType->isIntOrIntVectorTy() &&
-      CondVal->getType()->isVectorTy() == SelType->isVectorTy()) {
+      CondVal->getType()->isVectorTy() == SelType->isVectorTy() &&
+      CondVal->getType()->getScalarSizeInBits() < SelType->getScalarSizeInBits()) {
     // select C, 1, 0 -> zext C to int
     if (match(TrueVal, m_One()) && match(FalseVal, m_Zero()))
       return new ZExtInst(CondVal, SelType);
@@ -1399,6 +1399,7 @@
       return new SExtInst(NotCond, SelType);
     }
   }
+#endif

   // See if we are selecting two values based on a comparison of the two values.
   if (FCmpInst *FCI = dyn_cast<FCmpInst>(CondVal)) {
EOF
mkdir -p $llvm_builddir

cmake_flags=".. -DCMAKE_INSTALL_PREFIX=$llvm_installdir -DLLVM_ENABLE_ASSERTIONS=On -DLLVM_TARGETS_TO_BUILD=host -DCMAKE_BUILD_TYPE=$llvm_build_type"

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
