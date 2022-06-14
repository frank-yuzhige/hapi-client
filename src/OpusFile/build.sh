#!/bin/sh
set -ex

CURRDIR=$PWD
APINAME="opusfile"
APISOURCEDIR="test/test-api"
EXTRALIB="/lib"
TRACE_MAIN_IS="OpusFile.Trace"
UTILDIR="../Util"
HSINIT="hsinit.c"
GHCOPTS="-package hapi -L.""${EXTRALIB}"" -l:libopusfile.so.0 -optl-Wl,-rpath,./lib -optc--coverage -optl--coverage"
GHCOPTS_NOLINK="${GHCOPTS}"
GHCLIB=$(stack ghc -- --print-libdir)/include
GHCASAN="ghc-asan.sh"
GHCFUZZERNOLINK="ghc-fuzzer-no-link.sh"
GHCWRAPPER="ghc-wrapper.sh"


export PATH="/usr/bin:$PATH"
export PATH="${CURRDIR}""${EXTRALIB}"":$PATH"
# Build and install test API
mkdir -p "${EXTRALIB}"
cd "${CURRDIR}"/../../"${APISOURCEDIR}"/"${APINAME}"
make
cp -r ./.libs/ "${CURRDIR}"/"${EXTRALIB}"
cp -r ./include/ "${CURRDIR}"/"include"
cd "${CURRDIR}"

# Load GHC wrapper scripts and hsinit.c
for FILE in ${HSINIT} ${GHCWRAPPER} ${GHCASAN} ${GHCFUZZERNOLINK}
do
    cp "${UTILDIR}""/""${FILE}" "${FILE}"
done
clang -Wall -c -I"${GHCLIB}" -I"${CURRDIR}"/"include" "${HSINIT}"

sh "${GHCASAN}" "${GHCOPTS}" -c Example.hs Test.hs Debug.hs
sh "${GHCASAN}" "${GHCOPTS}" -no-hs-main -o test Example.o Test.o hsinit.o
sh "${GHCASAN}" "${GHCOPTS}" -no-hs-main -o debug Example.o Debug.o hsinit.o
sh "${GHCFUZZERNOLINK}" "${GHCOPTS}"" -main-is ""${TRACE_MAIN_IS}" -c Example.hs Trace.hs
sh "${GHCFUZZERNOLINK}" "${GHCOPTS}"" -main-is ""${TRACE_MAIN_IS}" -o trace Example.o Trace.o hsinit.o
