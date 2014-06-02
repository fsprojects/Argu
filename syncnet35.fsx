// --------------------------------------------------------------------------------------
// FAKE script to sync the UnionArgParse.Net35 with UnionArgParser (the 4.5 build proj). 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.MSBuild.ProjectSystem

seq { yield "./UnionArgParser/UnionArgParser.Net35.fsproj" }
|> FixProjectFiles "./UnionArgParser/UnionArgParser.fsproj" 
