#!/usr/bin/env bash

cd `dirname $0`

dotnet tool restore && \
dotnet fsi build.fsx "$@"