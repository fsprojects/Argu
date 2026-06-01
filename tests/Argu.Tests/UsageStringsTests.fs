namespace Argu.Tests

open Xunit
open Swensen.Unquote

open Argu

/// Tests for the localisable UsageStrings record (PR 15).
module ``Argu Tests UsageStrings`` =

    type SimpleArgs =
        | [<Mandatory>] Port of int
        | Verbose
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Port _ -> "port to bind to"
                | Verbose -> "verbose output"

    type SubArgs =
        | [<Mandatory>] Inner of string
        interface IArgParserTemplate with
            member this.Usage = "leaf"

    type WithSub =
        | [<CliPrefix(CliPrefix.None)>] Run of ParseResults<SubArgs>
        interface IArgParserTemplate with
            member this.Usage = "subcommand"

    [<Fact>]
    let ``UsageStrings.Default reproduces the historical English labels`` () =
        let d = UsageStrings.Default
        test <@ d.Usage = "USAGE: " @>
        test <@ d.Options = "OPTIONS:" @>
        test <@ d.Subcommands = "SUBCOMMANDS:" @>
        test <@ d.SubcommandHelpHint = "Use '{0} <subcommand> {1}' for additional information." @>

    [<Fact>]
    let ``PrintUsage with no override emits English headers`` () =
        let parser = ArgumentParser.Create<SimpleArgs>(programName = "app")
        let rendered = parser.PrintUsage()
        test <@ rendered.Contains "USAGE: " @>
        test <@ rendered.Contains "OPTIONS:" @>

    [<Fact>]
    let ``PrintUsage with custom UsageStrings emits translated headers`` () =
        let parser = ArgumentParser.Create<SimpleArgs>(programName = "app")
        let labels =
            { Usage = "UTILISATION : "
              Options = "OPTIONS :"
              Subcommands = "SOUS-COMMANDES :"
              SubcommandHelpHint = "Use '{0} <subcommand> {1}' for additional information." }
        let rendered = parser.PrintUsage(usageStrings = labels)
        test <@ rendered.Contains "UTILISATION : " @>
        test <@ rendered.Contains "OPTIONS :" @>
        // English labels must NOT appear when a custom set is provided.
        test <@ not (rendered.Contains "USAGE: ") @>
        test <@ not (rendered.Contains "OPTIONS:\r") @> // CRLF-anchored so it isn't matched by 'OPTIONS :'

    [<Fact>]
    let ``SubcommandHelpHint placeholders are interpolated`` () =
        let parser = ArgumentParser.Create<WithSub>(programName = "myapp")
        // Default English render mentions the program and help flag verbatim.
        let rendered = parser.PrintUsage()
        test <@ rendered.Contains "Use 'myapp <subcommand> --help'" @>

    [<Fact>]
    let ``Custom SubcommandHelpHint replaces the default hint line`` () =
        let parser = ArgumentParser.Create<WithSub>(programName = "myapp")
        let labels =
            { UsageStrings.Default with
                SubcommandHelpHint = "Tapez '{0} <sous-commande> {1}' pour plus d'informations." }
        let rendered = parser.PrintUsage(usageStrings = labels)
        test <@ rendered.Contains "Tapez 'myapp <sous-commande> --help' pour plus d'informations." @>
