#!/bin/sh

#
# USAGE:
#
# ./build-static.sh -tree /path/to/tree -snap myserver.snap
#

#
# SETTINGS:
#

SELF=/usr/local/bin/Self
BASE=../self/objects

$SELF -f $BASE/worldBuilder.self -b $BASE -f2 setup.self -o none -f3 loadwebsite.self -tree $1 -snap $2
