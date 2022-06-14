#!/bin/bash

FLAGS="-fsanitize=fuzzer-no-link,address -fprofile-instr-generate -fcoverage-mapping -fprofile-arcs -ftest-coverage --coverage"

./configure \
    CC=clang-13 \
    CXX=clang++-13 \
    CFLAGS="${FLAGS}" \
    CXXFLAGS="${FLAGS}"
