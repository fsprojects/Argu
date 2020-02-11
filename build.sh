#!/usr/bin/env bash

cd `dirname $0`

dotnet tool restore

# Use paket and not fake for restoring packages
# c.f. https://github.com/fsharp/FAKE/issues/2181
dotnet paket restore
export PAKET_SKIP_RESTORE_TARGETS=true

dotnet fake run build.fsx "$@"