#!/bin/sh
set -ex

CURRDIR=$PWD
APINAME="broken-arith"
REFNAME="good-arith"
APISOURCEDIR="test/test-api"
EXTRALIB="lib"
MAIN_IS="Stateless.Example"
UTILDIR="../Util"
HSINIT="hsinit.c"
GHCOPTS="-package hapi -L""${EXTRALIB}"" -l"${APINAME}" -l"${REFNAME}" -optl-Wl,-rpath,./lib -optc--coverage -optl--coverage"
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
cp -x lib"${REFNAME}".so "${CURRDIR}"/"${EXTRALIB}"
cd "${CURRDIR}"

# Load GHC wrapper scripts and hsinit.c
for FILE in ${HSINIT} ${GHCWRAPPER} ${GHCASAN} ${GHCFUZZERNOLINK}
do
    cp "${UTILDIR}""/""${FILE}" "${FILE}"
done
clang -Wall -c -I"${GHCLIB}" "${HSINIT}"

# sh "${GHCASAN}" "${GHCOPTS}" -c Example.hs QC.hs
# sh "${GHCFUZZERNOLINK}" -c LibFuzzer.hs
# sh "${GHCASAN}" "${GHCOPTS}" -no-hs-main -o test Example.o QC.o hsinit.o
# sh "${GHCASAN}" "${GHCOPTS}" -no-hs-main -o debug Example.o LibFuzzer.o hsinit.o
# sh "${GHCFUZZERNOLINK}" "${GHCOPTS}"" -main-is Stateless.LibFuzzer" -c Example.hs Trace.hs
# sh "${GHCFUZZERNOLINK}" "${GHCOPTS}"" -main-is Stateless.LibFuzzer" -o trace Example.o Trace.o hsinit.o

sh "${GHCWRAPPER}" "${GHCOPTS}"" -main-is Stateless.QC" -c Example.hs QC.hs
sh "${GHCWRAPPER}" "${GHCOPTS}"" -main-is Stateless.QC" -o qc Example.o QC.o
sh "${GHCASAN}" "${GHCOPTS}" -c Example.hs LibFuzzer.hs
sh "${GHCASAN}" "${GHCOPTS}" -no-hs-main -o test Example.o LibFuzzer.o hsinit.o
sh "${GHCFUZZERNOLINK}" "${GHCOPTS_NOLINK}"" -main-is Stateless.LibFuzzer" -c Example.hs LibFuzzer.hs
sh "${GHCFUZZERNOLINK}" "${GHCOPTS_NOLINK}"" -main-is Stateless.LibFuzzer" -o trace Example.o LibFuzzer.o hsinit.o
