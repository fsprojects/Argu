@echo off

dotnet tool restore
dotnet fake run build.fsx %*