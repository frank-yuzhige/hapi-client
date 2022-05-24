#!/bin/sh
set -ex

CURRDIR=$PWD
APINAME="broken-arith"
APISOURCEDIR="test/test-api"
EXTRALIB="lib"
MAIN_IS="Stateless.Example"
UTILDIR="../Util"
HSINIT="hsinit.c"
GHCOPTS="-package hapi -I""${EXTRALIB}"
GHCOPTS_NOLINK="${GHCOPTS}"" -main-is ""${MAIN_IS}"
GHCLIB=$(stack ghc -- --print-libdir)/include
GHCASAN="ghc-asan.sh"
GHCFUZZERNOLINK="ghc-fuzzer-no-link.sh"
GHCWRAPPER="ghc-wrapper.sh"


export PATH="/usr/bin:$PATH"
# Build and install test API
mkdir -p "${EXTRALIB}"
cd "${CURRDIR}"/../../"${APISOURCEDIR}"/"${APINAME}"
make all
cp -x lib"${APINAME}".so "${CURRDIR}"/"${EXTRALIB}"
cd "${CURRDIR}"

# Load GHC wrapper scripts and hsinit.c
for FILE in ${HSINIT} ${GHCWRAPPER} ${GHCASAN} ${GHCFUZZERNOLINK}
do
    cp "${UTILDIR}""/""${FILE}" "${FILE}"
done
clang -Wall -c -I"${GHCLIB}" "${HSINIT}"
sh "${GHCASAN}" "${GHCOPTS}" -c Example.hs
sh "${GHCASAN}" "${GHCOPTS}" -no-hs-main -o test Example.o hsinit.o
sh "${GHCFUZZERNOLINK}" "${GHCOPTS_NOLINK}" -c Example.hs -o ExampleTrace.o
sh "${GHCFUZZERNOLINK}" "${GHCOPTS_NOLINK}" -o trace ExampleTrace.o hsinit.o