#I "../../bin/net40"
#r "UnionArgParser.dll"
#r "UnionArgParser.Tests.dll"

open Nessos.UnionArgParser
open Nessos.UnionArgParser.Tests

let assembly = typeof<Argument>.Assembly

let results = parser.ParseAppSettings(assembly)

let args = results.GetAllResults()

parser.PrintAppSettings(Listener("localhost", 42) :: args, printComments = true)

parser.PrintCommandLine [Data [|1uy;2uy|] ; Record { Name = "me" ; Age = -1 }]

parser.Usage()