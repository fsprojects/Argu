#!/usr/bin/env bash

FAKE_FILE=build.fsx

set -eu
cd `dirname $0`

dotnet tool restore && \
dotnet paket restore # && \
# dotnet fake run $FAKE_FILE "$@"

exit_code=$?
if [ $exit_code -ne 0 ]; then
    exit $exit_code
fi

if [ "X$OS" = "XWindows_NT" ] ; then
  packages/build/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx 
else
  mono packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi