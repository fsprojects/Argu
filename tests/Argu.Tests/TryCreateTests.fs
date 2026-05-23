namespace Argu.Tests

open Xunit
open Swensen.Unquote

open Argu

/// Tests for ArgumentParser.TryCreate (PR 18).
module ``Argu Tests TryCreate`` =

    type ValidArgs =
        | [<Mandatory>] Port of int
        | Verbose
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Port _ -> "port to bind to"
                | Verbose -> "verbose output"

    /// Two union cases sharing the same CLI identifier trip
    /// checkUnionArgInfo's "conflicting CLI identifier" check.
    type BadCliNameArgs =
        | [<CustomCommandLine("--dup")>] Foo
        | [<CustomCommandLine("--dup")>] Bar
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Foo -> "first"
                | Bar -> "second"

    [<Fact>]
    let ``TryCreate returns Ok for a valid template`` () =
        match ArgumentParser.TryCreate<ValidArgs>(programName = "app") with
        | Ok p -> test <@ p.HelpFlags |> List.contains "--help" @>
        | Error e -> failwithf "expected Ok but got Error: %s" e.Message

    [<Fact>]
    let ``TryCreate returns Error for an invalid template`` () =
        match ArgumentParser.TryCreate<BadCliNameArgs>(programName = "app") with
        | Ok _ -> failwith "expected Error for template with conflicting CLI identifier"
        | Error e -> test <@ e.Message.Contains "--dup" || e.Message.Contains "conflicting" @>

    [<Fact>]
    let ``TryCreate Ok parser is functionally equivalent to Create`` () =
        let direct = ArgumentParser.Create<ValidArgs>(programName = "app")
        let viaTry =
            match ArgumentParser.TryCreate<ValidArgs>(programName = "app") with
            | Ok p -> p
            | Error e -> raise e
        // Same schema visible through both paths.
        test <@ direct.GetArgumentCases().Length = viaTry.GetArgumentCases().Length @>
        test <@ direct.HelpFlags = viaTry.HelpFlags @>

    [<Fact>]
    let ``TryCreate Error path does not throw`` () =
        // The whole point of TryCreate: a faulty template must not raise.
        let result = ArgumentParser.TryCreate<BadCliNameArgs>(programName = "app")
        test <@ (match result with Error _ -> true | Ok _ -> false) @>
