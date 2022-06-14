#!/bin/sh
set -ex

rm -f ./crash-*
rm -f ./leak-*
rm -f ./oom-*
find ./corpus -mindepth 1 -not -name "*.opus" -print0 | xargs -0 -r rm
rm -rf corpus-min
rm -f ./hapi_*
rm -f ./*.hi
rm -f ./*_stub.h
rm -f ./*.o
rm -rf ./lib
rm -f test
rm -f trace
rm -f debug
rm -f rep
rm -f rep.c
rm -f ghc-*.sh
rm -f *.profdata
rm -f *.profraw
rm -f hsinit.c
rm -rf ./include
