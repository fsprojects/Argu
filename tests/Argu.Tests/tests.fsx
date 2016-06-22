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
        member this.Usage = "push"

[<CliPrefix(CliPrefix.Dash)>]
type CleanArgs =
    | D
    | F
    | X
with
    interface IArgParserTemplate with
        member this.Usage = "clean"

[<CliPrefix(CliPrefix.None)>]
type GitArgs =
    | [<AltCommandLine("foo")>]Bar of int
    | Push of ParseResult<PushArgs>
    | Clean of ParseResult<CleanArgs>
with 
    interface IArgParserTemplate with 
        member this.Usage = "git"

let parser = ArgumentParser.Create<GitArgs>()

parser.PrintCommandLineFlat [Bar 42 ; Push(toParseResults [Remote "a b"])]

let result = parser.Parse([|"bar" ; "2" ; "clean" ; "-fdfx"|])
let nested = result.GetResult(<@ Clean @>)

result.Usage() |> System.Console.WriteLine
nested.Usage() |> System.Console.WriteLine

parser.PrintCommandLineSyntax()

parser.GetSubParser(<@ Clean @>).PrintCommandLineSyntax()