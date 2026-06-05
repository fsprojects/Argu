/// Tests for the localizable UsageStrings record
module Argu.Tests.UsageStrings

open Swensen.Unquote
open Xunit

open Argu

type SimpleArgs =
    | [<Mandatory>] Port of int
    | Verbose
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Port _ -> "port to bind to"
            | Verbose -> "verbose output"

type WithSub =
    | [<CliPrefix(CliPrefix.None)>] Run of ParseResults<SubArgs>
    interface IArgParserTemplate with
        member this.Usage = "subcommand"
and SubArgs =
    | [<Mandatory>] Inner of string
    interface IArgParserTemplate with
        member this.Usage = "leaf"

[<Fact>]
let ``UsageStrings.Default reproduces the historical English labels`` () =
    let d = UsageStrings.Default
    test <@ d.Usage = "USAGE: " @>
    test <@ d.Options = "OPTIONS:" @>
    test <@ d.Subcommands = "SUBCOMMANDS:" @>
    test <@ d.SubcommandHelpHintFormat = "Use '{0} <subcommand> {1}' for additional information." @>

[<Fact>]
let ``PrintUsage with no override emits English headers`` () =
    let parser = ArgumentParser.Create<SimpleArgs>()
    let rendered = parser.PrintUsage()
    test <@ rendered.Contains "USAGE:" @>
    test <@ rendered.Contains "OPTIONS:" @>

[<Fact>]
let ``PrintUsage with custom UsageStrings emits translated headers`` () =
    let parser = ArgumentParser.Create<SimpleArgs>()
    let labels =
        { Usage = "UTILISATION : "
          Options = "OPTIONS :"
          Subcommands = "SOUS-COMMANDES :"
          SubcommandHelpHintFormat = "Use '{0} <subcommand> {1}' for additional information." }
    let rendered = parser.PrintUsage(usageStrings = labels)
    test <@ rendered.Contains "UTILISATION : " @>
    test <@ rendered.Contains "OPTIONS :" @>
    // English labels must NOT appear when a custom set is provided.
    test <@ not (rendered.Contains "USAGE:") @>
    test <@ not (rendered.Contains "OPTIONS:") @>

[<Fact>]
let ``SubcommandHelpHint placeholders are interpolated`` () =
    let parser = ArgumentParser.Create<WithSub>(programName = "myapp")
    // Default English render mentions the program and help flag verbatim.
    let rendered = parser.PrintUsage()
    test <@ rendered.Contains "Use 'myapp <subcommand> --help'" @>

[<Fact>]
let ``Custom SubcommandHelpHint replaces the default hint line`` () =
    let parser = ArgumentParser.Create<WithSub>(programName = "myapp",
                                                (* Inhibit environment-induced wrapping*) usageStringCharacterWidth = 100)
    let labels =
        { UsageStrings.Default with
            SubcommandHelpHintFormat = "Tapez '{0} <sous-commande> {1}' pour plus d'informations." }
    let rendered = parser.PrintUsage(usageStrings = labels)
    test <@ rendered.Contains "Tapez 'myapp <sous-commande> --help' pour plus d'informations." @>

module Issue173 =

    type KanjiArgs =
        | Strokes of int
        | Min_Strokes of int
        | Max_Strokes of int
        | Include_Stroke_Miscounts
        | Radicals of string
        | Skip_Code of string
        | Sh_Code of string
        | Four_Corner_Code of string
        | Deroo_Code of string
        | Reading of string
        | Nanori of string
        | Common_Only
        | Pattern of string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Strokes _ -> ""
                | Min_Strokes _ -> ""
                | Max_Strokes _ -> "20"
                | Include_Stroke_Miscounts -> "false"
                | Radicals _ -> ""
                | Skip_Code _ -> ""
                | Sh_Code _ -> ""
                | Four_Corner_Code _ -> "92029"
                | Deroo_Code _ -> "92381"
                | Reading _ -> "xsuztlak"
                | Nanori _ -> ""
                | Common_Only -> "false"
                | Pattern _ -> "jjsoowi"

    [<Fact>]
    let ``Usage also prints new line for options that have an empty string as usage`` () =
        let parser = ArgumentParser<KanjiArgs>()
        let lines = parser.PrintUsage().Split("\n")
        let optionsLines: string[] = lines |> Array.skipWhile (fun s -> not (s.StartsWith UsageStrings.Default.Options))
        let args = [| "strokes"; "min-strokes"; "max-strokes"; "include-stroke-miscounts"; "radicals"; "skip-code";
                      "sh-code"; "four-corner-code"; "deroo-code"; "reading"; "nanori"; "common-only"; "pattern" |]
        test <@ args |> Array.forall (fun arg -> optionsLines |> Array.exists _.Contains($"--{arg}")) @>
