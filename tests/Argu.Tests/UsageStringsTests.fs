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
