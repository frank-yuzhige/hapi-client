#!/bin/bash
COUNTSLICE=10000
RESULT="./bench-count-results.txt"

./clean-corpus.sh
rm -f "${RESULT}"
touch "${RESULT}"

for I in {0..50}
do
    echo "Running time period no."$I""
    ./test -detect_leaks=0                \
           -rss_limit_mb=8192             \
           -dict=./opusfile.dict          \
           -ignore_crashes=1              \
           -runs="${CONTSLICE}" corpus || true
    echo "======== COUNT PERIOD NO."$I"" >> "${RESULT}"
    ./cov.sh >> "${RESULT}"
    echo "" >> "${RESULT}"
done
