#!/bin/sh
set -ex

CURRDIR=$PWD
APINAME="broken-arith"
APISOURCEDIR="test/test-api"
EXTRALIB="lib"
GHCOPTS="-package hapi -I"${EXTRALIB}
GHCLIB=$(stack ghc -- --print-libdir)/include

# Build and install test API
mkdir -p ${EXTRALIB}
cd ${CURRDIR}/../../${APISOURCEDIR}/${APINAME}
make all
cp -x lib${APINAME}.so ${CURRDIR}/${EXTRALIB}
cd ${CURRDIR}

clang -Wall -c -I${GHCLIB} hsinit.c
sh ghc-asan.sh ${GHCOPTS} -c Example.hs
sh ghc-asan.sh ${GHCOPTS} -no-hs-main -o test Example.o hsinit.o
