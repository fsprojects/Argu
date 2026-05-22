module Argu.Samples.Introspect.Main

// Demonstrates Argu's introspection API:
//   1. ArgumentParser.GetArgumentCases() returns ArgumentCaseInfo records
//      describing every case in the union template.
//   2. ArgumentParser.GetSubCommandParsers() returns untyped ArgumentParser
//      instances for every case whose payload is a ParseResults<_>.
//   3. IArgumentParserVisitor<'R> recovers the typed parser from the
//      untyped base, allowing rank-2 generic code over an unknown template.

open Argu
open Argu.Samples.Introspect.Arguments

// A visitor that just returns the parser's typed program name; useful
// to show how the visitor pattern bridges untyped → typed.
let typedProgramName (parser : ArgumentParser) =
    parser.Accept
        { new IArgumentParserVisitor<string> with
            member _.Visit<'T when 'T :> IArgParserTemplate> (typed : ArgumentParser<'T>) =
                typeof<'T>.Name }

// Walk a parser's schema, printing case metadata and recursing into subcommand parsers.
let rec dump (depth : int) (parser : ArgumentParser) =
    let indent = String.replicate depth "  "
    printfn "%sParser: %s (subcommand=%b)" indent (typedProgramName parser) parser.IsSubCommandParser
    for case in parser.GetArgumentCases() do
        let cliNames = case.CommandLineNames.Value |> String.concat ", "
        printfn "%s  - %s [%O] cli=[%s] mandatory=%b hidden=%b"
                indent
                case.Name.Value
                case.ArgumentType
                cliNames
                case.IsMandatory.Value
                case.IsHidden.Value
    for sub in parser.GetSubCommandParsers() do
        dump (depth + 1) sub

[<EntryPoint>]
let main _argv =
    let parser = ArgumentParser.Create<GitArgs>(programName = "git")
    printfn "=== Schema for 'git' ==="
    dump 0 parser
    printfn ""
    printfn "=== Help text ==="
    printfn "%s" (parser.PrintUsage())
    0
