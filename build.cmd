@echo off

dotnet tool restore
dotnet fsi build.fsx %*