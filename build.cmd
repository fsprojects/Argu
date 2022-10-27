@echo off

export PAKET_SKIP_RESTORE_TARGETS=true

dotnet tool restore
dotnet paket restore
dotnet fsi build.fsx %*