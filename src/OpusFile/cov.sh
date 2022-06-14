#!/bin/bash

TEST="./test"
CORPUS="./corpus"
CORPUS_MIN="./corpus-min"
PROFDATA="default.profdata"

# need to exclude any crash inputs when generating code coverage report (damn!)
rm -rf "${CORPUS_MIN}"
mkdir -p "${CORPUS_MIN}"
ASAN_OPTIONS=detect_leaks=0 ${TEST} -detect_leaks=0 -merge=1 ${CORPUS_MIN} ${CORPUS} # minimize corpus
ASAN_OPTIONS=detect_leaks=0 ${TEST} -detect_leaks=0 -runs=0 ${CORPUS_MIN}            # dry-run to generate coverage

llvm-profdata merge -sparse *.profraw -o "${PROFDATA}"
llvm-cov report \
    -instr-profile "${PROFDATA}" \
    -object ./lib/libopusfile.so \
    -object ./lib/libopusfile.so.0 \
    -object Example.o \
    -object Test.o \
