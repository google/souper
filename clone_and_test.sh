#!/bin/bash -ex

# get travis env
echo "TRAVIS_EVENT_TYPE: ${TRAVIS_EVENT_TYPE}"
echo "TRAVIS_BRANCH: ${TRAVIS_BRANCH}"
echo "TRAVIS_REPO_SLUG: ${TRAVIS_REPO_SLUG}"
echo "TRAVIS_PULL_REQUEST_SLUG: ${TRAVIS_PULL_REQUEST_SLUG}"
echo "TRAVIS_PULL_REQUEST_BRANCH: ${TRAVIS_PULL_REQUEST_BRANCH}"

# set default variable if not in env
if [ -z ${TRAVIS_EVENT_TYPE} ]; then
TRAVIS_EVENT_TYPE="push";
echo "TRAVIS_EVENT_TYPE set to push";
fi

if [ -z ${TRAVIS_BRANCH} ]; then
TRAVIS_BRANCH="master";
echo "TRAVIS_BRANCH set to master";
fi

# check if this is a pull request or a push
if [ ${TRAVIS_EVENT_TYPE} == "pull_request" ]; then
    git clone https://github.com/${TRAVIS_PULL_REQUEST_SLUG} souper-test;
    cd souper-test;
    git checkout ${TRAVIS_PULL_REQUEST_BRANCH};
    ln -s /usr/src/souper/third_party;
else
    # it's a push
    git clone https://github.com/${TRAVIS_REPO_SLUG} souper-test;
    cd souper-test;
    git checkout ${TRAVIS_BRANCH};
    ln -s /usr/src/souper/third_party;
fi

export GOPATH=/usr/src/go
Z3=/usr/bin/z3
SRCDIR="$PWD"

# invoke run_lit directly since ninja buffers output causing Travis to
# give up after 10 minutes

mkdir build-release
cd build-release
PATH=/usr/src/souper/third_party/llvm/Release/bin:$PATH cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_FLAGS='-Werror' -DTEST_SOLVER=-z3-path=$Z3 -DTEST_SYNTHESIS=ON -DTEST_LONG_DURATION_SYNTHESIS=OFF -DCMAKE_BUILD_TYPE=Release ..
ninja
LIT_ARGS="-v -vv" ./run_lit
#LIT_ARGS="-v -vv --vg --vg-arg=--trace-children-skip=$Z3 --vg-arg=--suppressions=$SRCDIR/valgrind.supp" ./run_lit
cd ..

# TODO: turn on ASan as well, which requires LLVM to be built with it
# in order to get our pass to work

#mkdir build-release-sanitize
#cd build-release-sanitize
#PATH=/usr/src/souper/third_party/llvm/Release/bin:$PATH cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_FLAGS='-fsanitize=undefined -fno-sanitize-recover=all -Werror' -D#TEST_SOLVER=-z3-path=$Z3 -DTEST_SYNTHESIS=ON -DTEST_LONG_DURATION_SYNTHESIS=OFF -DCMAKE_BUILD_TYPE=Release ..
#ninja
#LIT_ARGS="-v -vv" ./run_lit
#cd ..

#mkdir build-debug
#cd build-debug
#cmake -G Ninja -DTEST_SOLVER=-z3-path=$Z3 -DTEST_SYNTHESIS=ON -DCMAKE_BUILD_TYPE=Debug ..
#ninja
#LIT_ARGS="-v -vv" ninja check
#LIT_ARGS="-v -vv --vg --vg-arg=--trace-children-skip=$Z3 --vg-arg=--suppressions=$SRCDIR/valgrind.supp" ninja check
#cd ..
