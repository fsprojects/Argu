@echo off

dotnet tool restore

REM Use paket and not fake for restoring packages
REM c.f. https://github.com/fsharp/FAKE/issues/2181
dotnet paket restore
export PAKET_SKIP_RESTORE_TARGETS=true

dotnet fake run build.fsx %*