#!/usr/bin/env bash

set -eu
cd `dirname $0`

dotnet tool restore
dotnet paket restore
dotnet fake run build.fsx "$@"