#I "../../bin/net40"
#r "Argu.dll"
#r "Argu.Tests.dll"

open Argu
open Argu.Tests

type PushArgs =
    | Remote of name:string
    | Branch of name:string
with
    interface IArgParserTemplate with
        member this.Usage = "foo bar foo bar foo bar"

[<CliPrefix(CliPrefix.Dash)>]
type CleanArgs =
    | D
    | F
    | X
with
    interface IArgParserTemplate with
        member this.Usage = "foo bar foo bar foo bar"

[<CliPrefix(CliPrefix.None)>]
type GitArgs =
    | [<AltCommandLine("foo")>]Bar of int
    | [<AltCommandLine("-p")>]Push of ParseResult<PushArgs>
    | Clean of ParseResult<CleanArgs>
    | [<CustomCommandLine("-E")>][<EqualsAssignment>]Env of key:string * value:string
    | Optional of string option
    | List of int list
with 
    interface IArgParserTemplate with 
        member this.Usage = "foo bar foo bar foo bar"

let parser = ArgumentParser.Create<GitArgs>(programName = "gadget", description = "Gadget -- my awesome CLI tool")

parser.PrintCommandLineFlat [Bar 42 ; Push(toParseResults [Remote "a b"])]

let result = parser.Parse [|"bar" ; "2" ; "optional" ; "test" ; "list" ; "-1" ; "1821" ; "781" ; "-E" ; "A=12" ; "clean" ; "-fdfx"|]
result.GetAllResults()
let nested = result.GetResult(<@ Clean @>)

result.Usage() |> System.Console.WriteLine
nested.Usage() |> System.Console.WriteLine

parser.PrintCommandLineSyntax()

parser.GetSubParser(<@ Clean @>).PrintCommandLineSyntax()