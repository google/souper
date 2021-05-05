from ubuntu:20.04

run set -x; \
    echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections \
    && apt-get update -y -qq \
    && apt-get dist-upgrade -y -qq \
    && apt-get autoremove -y -qq \
    && apt-get remove -y -qq clang llvm llvm-runtime \
    && apt-get install libgmp10 \
    && echo 'ca-certificates valgrind libc6-dev libgmp-dev cmake ninja-build make autoconf automake libtool python python3 subversion re2c git clang libstdc++-10-dev redis' > /usr/src/build-deps \
    && apt-get install -y $(cat /usr/src/build-deps) --no-install-recommends

add build_deps.sh /usr/src/souper/build_deps.sh
add clone_and_test.sh /usr/src/souper/clone_and_test.sh

run export CC=clang CXX=clang++ \
    && cd /usr/src/souper \
#   && ./build_deps.sh Debug \
#   && rm -r third_party/llvm-Debug-build \
    && ./build_deps.sh Release \
    && rm -r third_party/llvm-Release-build


add CMakeLists.txt /usr/src/souper/CMakeLists.txt
add docs /usr/src/souper/docs
add include /usr/src/souper/include
add lib /usr/src/souper/lib
add runtime /usr/src/souper/runtime
add test /usr/src/souper/test
add tools /usr/src/souper/tools
add utils /usr/src/souper/utils
add unittests /usr/src/souper/unittests

run export LD_LIBRARY_PATH=/usr/src/souper/third_party/z3-install/lib:$LD_LIBRARY_PATH \
    && mkdir -p /usr/src/souper-build \
    && cd /usr/src/souper-build \
    && CC=/usr/src/souper/third_party/llvm-Release-install/bin/clang CXX=/usr/src/souper/third_party/llvm-Release-install/bin/clang++ cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DTEST_SYNTHESIS=ON ../souper \
    && ninja \
    && ninja check \
    && cd .. \
    && rm -rf /usr/src/souper-build \
    && groupadd -r souper \
    && useradd -m -r -g souper souper \
    && mkdir /data \
    && chown souper:souper /data \
    && rm -rf /usr/local/include /usr/local/lib/*.a /usr/local/lib/*.la
