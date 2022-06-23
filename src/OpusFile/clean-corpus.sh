#!/bin/sh
set -ex

find ./corpus -mindepth 1 -not -name "*.opus" -print0 | xargs -0 -r rm
