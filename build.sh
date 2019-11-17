#!/usr/bin/env bash

FAKE_FILE=build.fsx

set -eu
cd `dirname $0`

dotnet tool restore && \
dotnet paket restore && \
dotnet fake run $FAKE_FILE "$@"