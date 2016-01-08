#I "../../bin/net40"
#r "Argu.dll"
#r "Argu.Tests.dll"

open Argu
open Argu.Tests

let assembly = typeof<Argument>.Assembly

let results = parser.ParseAppSettings(assembly)

let args = results.GetAllResults()

parser.PrintAppSettings(Listener("localhost", 42) :: args, printComments = true)

parser.PrintCommandLine [Data(1, [|1uy;2uy|])]

parser.Usage()

type Args =
    | [<Mandatory>] X of string
with 
    interface IArgParserTemplate with 
        member this.Usage = ""

let parser = Argu.Create<Args>()

parser.Parse(raiseOnUsage = false, inputs = [|"--help"|])