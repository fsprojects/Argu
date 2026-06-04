/// exact error-message snapshots, AppSettings edge cases, and deep-subcommand mandatory-missing.
/// These are behavior-locking tests, not exhaustive functional tests.
module Argu.Tests.CoverageTests

open System
open System.Collections.Generic
open Swensen.Unquote
open Xunit

open Argu

[<RequireQualifiedAccess>]
module Helpers =
    let parserError (parser : ArgumentParser<'T>) (argv : string []) =
        try
            parser.ParseCommandLine(argv, raiseOnUsage = true) |> ignore
            None
        with :? ArguParseException as e -> Some (e.ErrorCode, e.Message)

    let firstLine (msg : string) =
        msg.Split([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries).[0]


// === Error message exact-text snapshots ===

type SimpleArgs =
    | [<Mandatory>] Port of int
    | Verbose
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Port _ -> "port to bind to"
            | Verbose -> "verbose output"

[<Fact>]
let ``ErrorCode.CommandLine: unrecognized argument message`` () =
    let parser = ArgumentParser.Create<SimpleArgs>(programName = "app")
    match Helpers.parserError parser [| "--bogus" |] with
    | None -> failwith "expected parse error"
    | Some (code, msg) ->
        test <@ code = ErrorCode.CommandLine @>
        test <@ Helpers.firstLine msg = "ERROR: unrecognized argument: '--bogus'." @>

[<Fact>]
let ``ErrorCode.PostProcess: missing mandatory message`` () =
    let parser = ArgumentParser.Create<SimpleArgs>(programName = "app")
    match Helpers.parserError parser [||] with
    | None -> failwith "expected parse error"
    | Some (code, msg) ->
        test <@ code = ErrorCode.PostProcess @>
        test <@ Helpers.firstLine msg = "ERROR: missing parameter '--port'." @>

[<Fact>]
let ``ErrorCode.CommandLine: missing argument value message`` () =
    let parser = ArgumentParser.Create<SimpleArgs>(programName = "app")
    match Helpers.parserError parser [| "--port" |] with
    | None -> failwith "expected parse error"
    | Some (code, msg) ->
        test <@ code = ErrorCode.CommandLine @>
        test <@ (Helpers.firstLine msg).StartsWith "ERROR: argument '--port' must be followed by" @>


// === Deep (3-level) subcommand mandatory-missing ===

type Level3Args =
    | [<Mandatory>] Inner of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Inner _ -> "innermost mandatory leaf"

type Level2Args =
    | [<CliPrefix(CliPrefix.None)>] Level3 of ParseResults<Level3Args>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Level3 _ -> "level-3 subcommand"

type Level1Args =
    | [<CliPrefix(CliPrefix.None)>] Level2 of ParseResults<Level2Args>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Level2 _ -> "level-2 subcommand"

[<Fact>]
let ``Deep subcommand: mandatory at level 3 missing surfaces in error`` () =
    let parser = ArgumentParser.Create<Level1Args>(programName = "app")
    match Helpers.parserError parser [| "level2"; "level3" |] with
    | None -> failwith "expected parse error"
    | Some (code, msg) ->
        test <@ code = ErrorCode.PostProcess @>
        // 'inner' is the deepest mandatory; its name should appear in the message.
        test <@ msg.Contains "--inner" @>

[<Fact>]
let ``Deep subcommand: providing the leaf mandatory parses cleanly`` () =
    let parser = ArgumentParser.Create<Level1Args>(programName = "app")
    let results = parser.ParseCommandLine([| "level2"; "level3"; "--inner"; "ok" |], raiseOnUsage = false)
    // Probe through the public surface: walk subcommand → subcommand → leaf.
    let l2 = results.GetResult(Level2)
    let l3 = l2.GetResult(Level3)
    test <@ l3.GetResult(Inner) = "ok" @>


// === AppSettings edge cases ===

type AppSettingsArgs =
    | [<CustomAppSettings("required-key"); Mandatory>] RequiredKey of string
    | [<CustomAppSettings("optional-key")>] OptionalKey of string
    | [<CustomAppSettings("int-key")>] IntKey of int
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | RequiredKey _ -> "mandatory configuration key"
            | OptionalKey _ -> "optional configuration key"
            | IntKey _ -> "typed-int configuration key"

[<Fact>]
let ``AppSettings: dictionary reader returns parsed value`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>(programName = "app")
    let dict = Dictionary<string, string>()
    dict["required-key"] <- "hello"
    dict["int-key"] <- "42"
    let reader = ConfigurationReader.FromDictionary dict
    let results = parser.ParseConfiguration(reader, ignoreMissing = false)
    test <@ results.GetResult(RequiredKey) = "hello" @>
    test <@ results.GetResult(IntKey) = 42 @>

[<Fact>]
let ``AppSettings: missing mandatory key raises`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>(programName = "app")
    let dict = Dictionary<string, string>()
    // required-key intentionally absent
    let reader = ConfigurationReader.FromDictionary dict
    let raised =
        try parser.ParseConfiguration(reader, ignoreMissing = false) |> ignore ; None
        with :? ArguParseException as e -> Some (e.ErrorCode, e.Message)
    match raised with
    | None -> failwith "expected parse error"
    | Some (code, msg) ->
        test <@ code = ErrorCode.PostProcess @>
        // Argu reports the CLI name in the missing-mandatory error, not the
        // AppSettings key. The CLI name is auto-derived ("requiredkey").
        test <@ msg.Contains "--requiredkey" @>

[<Fact>]
let ``AppSettings: null or empty value is treated as absent`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>(programName = "app")
    let dict = Dictionary<string, string>()
    dict["required-key"] <- "x" // satisfy mandatory
    dict["optional-key"] <- ""   // empty string should be treated as absent
    let reader = ConfigurationReader.FromDictionary dict
    let results = parser.ParseConfiguration(reader, ignoreMissing = false)
    test <@ results.TryGetResult(OptionalKey) = None @>

[<Fact>]
let ``AppSettings: invalid type-parse raises with key in message`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>(programName = "app")
    let dict = Dictionary<string, string>()
    dict["required-key"] <- "x"
    dict["int-key"] <- "not-a-number"
    let reader = ConfigurationReader.FromDictionary dict
    let raised =
        try parser.ParseConfiguration(reader, ignoreMissing = false) |> ignore ; None
        with :? ArguParseException as e -> Some (e.ErrorCode, e.Message)
    match raised with
    | None -> failwith "expected parse error"
    | Some (code, msg) ->
        test <@ code = ErrorCode.AppSettings @>
        test <@ msg.Contains "int-key" @>

[<Fact>]
let ``AppSettings: ignoreMissing=true skips mandatory check`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>(programName = "app")
    let reader = ConfigurationReader.FromDictionary(Dictionary<string, string>())
    // Should not raise.
    let results = parser.ParseConfiguration(reader, ignoreMissing = true)
    test <@ results.TryGetResult(RequiredKey) = None @>


// === Env-var (covers Argu's existing EnvironmentVariableConfigurationReader) ===

type EnvArgs =
    | [<CustomAppSettings("ARGU_TEST_VAL")>] Val of string
    interface IArgParserTemplate with
        member this.Usage = "value"

[<Fact>]
let ``EnvironmentVariableConfigurationReader: reads process env`` () =
    let key = "ARGU_TEST_VAL"
    let prior = Environment.GetEnvironmentVariable key
    try
        Environment.SetEnvironmentVariable(key, "from-env")
        let parser = ArgumentParser.Create<EnvArgs>(programName = "app")
        let reader = ConfigurationReader.FromEnvironmentVariables()
        let results = parser.ParseConfiguration(reader, ignoreMissing = true)
        test <@ results.GetResult(Val) = "from-env" @>
    finally
        Environment.SetEnvironmentVariable(key, prior)
