#!/bin/bash

PROFDATA="default.profdata"

llvm-profdata merge -sparse *.profraw -o "${PROFDATA}"
llvm-cov report \
    -instr-profile "${PROFDATA}" \
    -object ./.libs/libopusfile.so
    -object ./.libs/libopusfile.so.0
