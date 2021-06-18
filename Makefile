# Copyright 2019 The Souper Authors. All rights reserved.
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

# Invoke with `make deps Build=Debug` or `make deps` (defaults to Release)  

llvm_branch=tags/RELEASE_700/final
llvm_build_type=$(Build)
alivedir=third_party/alive2
alive_builddir=$(alivedir)/build
hiredis_commit=010756025e8cefd1bc66c6d4ed3b1648ef6f1f95
klee_repo=https://github.com/rsas/klee
klee_branch=pure-bv-qf-llvm-7.0
hiredisdir=third_party/hiredis
kleedir=third_party/klee
llvmdir=third_party/llvm
llvm_installdir=$(llvmdir)/$(llvm_build_type)
llvm_builddir=$(llvmdir)/${llvm_build_type}-build
cmake_flags=-DCMAKE_INSTALL_PREFIX="../../../$(llvm_installdir)" -DLLVM_ENABLE_ASSERTIONS=On -DLLVM_TARGETS_TO_BUILD=host -DCMAKE_BUILD_TYPE="$(llvm_build_type)"
Build=Release
# $(Build) will not be set here if assigned through the command line
# TODO: Make the targets build-type specific, currently you have to nuke third_party for changing build type
NINJA=ninja
# Override NINJA from the command line with something like NINJA="ninja -j1" if you are running out of memory

deps : alive2 llvm hiredis klee

llvm : third_party/llvm llvm-build
# 	we want these but they don't get installed by default
	cp $(llvm_builddir)/bin/llvm-lit $(llvm_installdir)/bin/
	cp $(llvm_builddir)/bin/FileCheck $(llvm_installdir)/bin/
	cp $(llvm_builddir)/lib/libgtest_main.a $(llvm_installdir)/lib/
	cp $(llvm_builddir)/lib/libgtest.a $(llvm_installdir)/lib/

llvm-build:
	@echo "Building LLVM"

	mkdir -p $(llvm_builddir)
	
	if [ -n "`which ninja`" ]; then\
		$(NINJA) -C $(llvm_builddir);\
		$(NINJA) -C $(llvm_builddir) install;\
	else\
		make -C $(llvm_builddir) -j4;\
		make -C $(llvm_builddir) -j4 install;\
	fi
alive2 : third_party/alive2
	@echo "Building alive2"
	mkdir -p $(alivedir) $(alive_builddir)
	if [ -n "`which ninja`" ]; then\
		(cd $(alive_builddir) && cmake ../alive2 -DCMAKE_BUILD_TYPE=$(llvm_build_type) -GNinja) && $(NINJA) -C $(alive_builddir);\
	else\
		(cd $(alive_builddir) && cmake ../alive2 -DCMAKE_BUILD_TYPE=$(llvm_build_type)) && make -C $(alive_builddir) -j4;\
	fi

hiredis : third_party/hiredis
	@echo "Installing hiredis"
	(cd $(hiredisdir) && git fetch)
	mkdir -p $(hiredisdir)/install/include/hiredis
	mkdir -p $(hiredisdir)/install/lib
	(cd $(hiredisdir) && git checkout $(hiredis_commit) && make libhiredis.a &&\
	cp -r hiredis.h async.h read.h sds.h adapters install/include/hiredis &&\
	cp libhiredis.a install/lib)

klee : third_party/klee
	(cd $(kleedir) && git fetch)


third_party/llvm : patches/disable-instcombine-select-to-logic.patch patches/enable-instcombine-switch.patch
	@echo "Fetching LLVM"
	mkdir -p third_party/llvm
	svn co https://llvm.org/svn/llvm-project/llvm/${llvm_branch} $(llvmdir)
	svn co https://llvm.org/svn/llvm-project/cfe/${llvm_branch} $(llvmdir)/tools/clang
	svn co https://llvm.org/svn/llvm-project/compiler-rt/${llvm_branch} $(llvmdir)/projects/compiler-rt
	svn co https://llvm.org/svn/llvm-project/libcxx/${llvm_branch} $(llvmdir)/projects/libcxx
	svn co https://llvm.org/svn/llvm-project/libcxxabi/${llvm_branch} $(llvmdir)/projects/libcxxabi
	
	@echo "Patching LLVM"
	# Disable the broken select -> logic optimizations
	patch $(llvmdir)/lib/Transforms/InstCombine/InstCombineSelect.cpp < patches/disable-instcombine-select-to-logic.patch
	# Apply instcombine switch patch
	patch -d $(llvmdir) -p0 -i ../../patches/enable-instcombine-switch.patch
	
	@echo "Configuring LLVM"

	mkdir -p $(llvm_builddir)
	
	if [ -n "`which ninja`" ]; then\
		(cd $(llvm_builddir) && cmake ../ -G Ninja $(cmake_flags));\
	else\
		(cd $(llvm_builddir) && cmake ../ $(cmake_flags));\
	fi

third_party/alive2 : 
	@echo "Fetching alive2"
	llvm_build_type=$(Build)
	mkdir -p third_party/alive2
	git clone https://github.com/manasij7479/alive2.git $(alivedir)/alive2

third_party/hiredis : 
	@echo "Fetching hiredis"
	mkdir -p third_party/hiredis
	git clone https://github.com/redis/hiredis.git $(hiredisdir)

third_party/klee : 
	@echo "Fetching KLEE"
	git clone -b $(klee_branch) $(klee_repo) $(kleedir)
