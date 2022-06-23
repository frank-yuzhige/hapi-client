#!/bin/bash
TIMESLICE=10
RESULT="./bench-results-no-corpus.txt"

rm -f "${RESULT}"
touch "${RESULT}"

for I in {1..30}
do
    echo "Running time period no."$I""
    ./test -detect_leaks=0                \
           -rss_limit_mb=8192             \
           -dict=./opusfile.dict          \
           -ignore_crashes=1              \
           -close_fd_mask=1               \
           -max_total_time="${TIMESLICE}" || true
    echo "======== TIME PERIOD NO."$I"" >> "${RESULT}"
    ./cov.sh >> "${RESULT}"
    echo "" >> "${RESULT}"
done
