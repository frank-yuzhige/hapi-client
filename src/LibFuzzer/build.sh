#!/bin/sh
set -ex

CURRDIR=$PWD
APINAME="broken-arith"
APISOURCEDIR="test/test-api"
EXTRALIB="lib"
MAIN_IS="LibFuzzer.Example"
GHCOPTS="-package hapi -I"${EXTRALIB}
GHCOPTS_NOLINK=${GHCOPTS}" -main-is "${MAIN_IS}
GHCLIB=$(stack ghc -- --print-libdir)/include

export PATH="/usr/bin:$PATH"
# Build and install test API
mkdir -p ${EXTRALIB}
cd ${CURRDIR}/../../${APISOURCEDIR}/${APINAME}
make all
cp -x lib${APINAME}.so ${CURRDIR}/${EXTRALIB}
cd ${CURRDIR}

clang -Wall -c -I${GHCLIB} hsinit.c
sh ghc-asan.sh ${GHCOPTS} -c Example.hs
sh ghc-asan.sh ${GHCOPTS} -no-hs-main -o test Example.o hsinit.o
sh ghc-fuzzer-no-link.sh ${GHCOPTS_NOLINK} -c Example.hs -o ExampleTrace.o
sh ghc-fuzzer-no-link.sh ${GHCOPTS_NOLINK} -o trace ExampleTrace.o hsinit.o
