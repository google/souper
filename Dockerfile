from ubuntu:14.04

run set -x; \
	apt-get update \
	&& apt-get install libantlr3c-3.2-0 libgmp10 \
	&& echo 'ca-certificates gcc g++ antlr3 libantlr3c-dev libboost-dev libc6-dev libgmp-dev cmake ninja-build make autoconf automake libtool golang-go python subversion git' > /usr/src/build-deps \
	&& apt-get install -y $(cat /usr/src/build-deps) --no-install-recommends \
	&& git clone https://github.com/CVC4/CVC4 /usr/src/cvc4 \
	&& git clone https://github.com/antirez/redis /usr/src/redis

run cd /usr/src/cvc4 \
	&& git checkout 1.4 \
	&& ./autogen.sh \
	&& mkdir -p /usr/src/cvc4-build \
	&& cd /usr/src/cvc4-build \
	&& ../cvc4/configure --disable-shared \
	&& make -j4 \
	&& make install

run cd /usr/src/redis \
	&& git checkout 2.8.13 \
	&& make -j4 \
	&& make install

run export GOPATH=/usr/src/go \
	&& go get github.com/garyburd/redigo/redis

add update_deps.sh /usr/src/souper/update_deps.sh

run cd /usr/src/souper \
	&& ./update_deps.sh Release \
	&& rm -rf third_party/hiredis/install/lib/libhiredis.so*

add CMakeLists.txt /usr/src/souper/CMakeLists.txt
add docs /usr/src/souper/docs
add include /usr/src/souper/include
add lib /usr/src/souper/lib
add test /usr/src/souper/test
add tools /usr/src/souper/tools
add unittests /usr/src/souper/unittests

run export GOPATH=/usr/src/go \
	&& mkdir -p /usr/src/souper-build \
	&& cd /usr/src/souper-build \
	&& CC=/usr/src/souper/third_party/llvm/Release/bin/clang CXX=/usr/src/souper/third_party/llvm/Release/bin/clang++ cmake -G Ninja -DCMAKE_BUILD_TYPE=Release ../souper \
	&& ninja souperweb souperweb-backend \
	&& cp souperweb souperweb-backend /usr/local/bin \
	&& strip /usr/local/bin/* \
	&& groupadd -r souper \
	&& useradd -r -g souper souper \
	&& mkdir /data \
	&& chown souper:souper /data \
	&& apt-get purge -y $(cat /usr/src/build-deps) \
	&& apt-get autoremove -y \
	&& rm -rf /usr/src /usr/local/include /usr/local/lib/*.a /usr/local/lib/*.la
