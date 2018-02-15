#!/usr/bin/env bash

# From https://github.com/scala-native/scala-native/blob/master/bin/travis_setup.sh

# Enable strict mode and fail the script on non-zero exit code,
# unresolved variable or pipe failure.
set -euo pipefail
IFS=$'\n\t'

# Remove pre-bundled libunwind
sudo find /usr -name "*libunwind*" -delete

# Use pre-bundled clang
export PATH=/usr/local/clang-5.0.0/bin:$PATH
export CXX=clang++

# Install Boehm GC and libunwind
sudo apt-get -qq update
sudo apt-get -y install libgc-dev
sudo apt-get -y install libunwind8-dev

# Build and install re2 from source
git clone https://code.googlesource.com/re2
pushd re2
git checkout 2017-03-01
make -j4 test
sudo make install prefix=/usr
make testinstall prefix=/usr
popd