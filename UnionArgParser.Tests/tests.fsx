#r "bin/Debug/UnionArgParser.dll"
#r "bin/Debug/UnionArgParser.Tests.dll"

open Nessos.UnionArgParser
open Nessos.UnionArgParser.Tests

let assembly = typeof<Argument>.Assembly

let results = parser.ParseAppSettings(assembly)

let args = results.GetAllResults()