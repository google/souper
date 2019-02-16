from ubuntu:18.04

run set -x; \
        apt-get update -qq \
        && apt-get dist-upgrade -qq \
        && apt-get autoremove -qq \
        && apt-get remove -y -qq clang llvm llvm-runtime \
	&& apt-get install libgmp10 \
	&& echo 'ca-certificates valgrind libc6-dev libgmp-dev cmake patch ninja-build make autoconf automake libtool golang-go python subversion re2c git clang' > /usr/src/build-deps \
	&& apt-get install -y $(cat /usr/src/build-deps) --no-install-recommends \
	&& git clone https://github.com/antirez/redis /usr/src/redis

run export CC=clang CXX=clang++ \
        && cd /usr/src/redis \
	&& git checkout 5.0.3 \
	&& make -j10 \
	&& make install

run export GOPATH=/usr/src/go \
	&& go get github.com/gomodule/redigo/redis

add build_deps.sh /usr/src/souper/build_deps.sh
add clone_and_test.sh /usr/src/souper/clone_and_test.sh
add patches /usr/src/souper/patches

run export CC=clang CXX=clang++ \
	&& cd /usr/src/souper \
#	&& ./build_deps.sh Debug \
#       && rm -rf third_party/llvm/Debug-build \
	&& ./build_deps.sh Release \
        && rm -rf third_party/llvm/Release-build \
	&& rm -rf third_party/hiredis/install/lib/libhiredis.so*

add CMakeLists.txt /usr/src/souper/CMakeLists.txt
add docs /usr/src/souper/docs
add include /usr/src/souper/include
add lib /usr/src/souper/lib
add test /usr/src/souper/test
add tools /usr/src/souper/tools
add utils /usr/src/souper/utils
add runtime /usr/src/souper/runtime
add unittests /usr/src/souper/unittests

run export GOPATH=/usr/src/go \
	&& mkdir -p /usr/src/souper-build \
	&& cd /usr/src/souper-build \
	&& CC=/usr/src/souper/third_party/llvm/Release/bin/clang CXX=/usr/src/souper/third_party/llvm/Release/bin/clang++ cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DTEST_SYNTHESIS=ON ../souper \
	&& ninja souperweb souperweb-backend \
        && ninja check \
	&& cp souperweb souperweb-backend /usr/local/bin \
        && cd .. \
        && rm -rf /usr/src/souper-build \
	&& strip /usr/local/bin/* \
	&& groupadd -r souper \
	&& useradd -m -r -g souper souper \
	&& mkdir /data \
	&& chown souper:souper /data \
	&& rm -rf /usr/local/include /usr/local/lib/*.a /usr/local/lib/*.la
