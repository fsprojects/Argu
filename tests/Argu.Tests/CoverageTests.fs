/// exact error-message snapshots, AppSettings edge cases, and deep-subcommand mandatory-missing.
/// These are behavior-locking tests, not exhaustive functional tests.
module Argu.Tests.CoverageTests

open Swensen.Unquote
open System
open System.Collections.Generic
open Xunit

open Argu

type Exception with
    member inline x.FirstLine = x.Message.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries).[0]

// === Error message exact-text snapshots ===

type SimpleArgs =
    | [<Mandatory>] Port of int
    | Verbose
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Port _ -> "port to bind to"
            | Verbose -> "verbose output"

let run<'T when 'T :> IArgParserTemplate> argv = ArgumentParser.Create<'T>().ParseCommandLine argv

[<Fact>]
let ``ErrorCode.CommandLine: unrecognized argument message`` () =
    raisesWith<ArguParseException>
        <@ run<SimpleArgs> [| "--bogus" |] @>
        <| fun e -> <@ e.ErrorCode = ErrorCode.CommandLine
                        && e.FirstLine = "ERROR: unrecognized argument: '--bogus'." @>

[<Fact>]
let ``ErrorCode.PostProcess: missing mandatory message`` () =
    raisesWith<ArguParseException>
        <@ run<SimpleArgs> [||] @>
        <| fun e -> <@ e.ErrorCode = ErrorCode.PostProcess
                     && e.FirstLine = "ERROR: missing parameter '--port'." @>

[<Fact>]
let ``ErrorCode.CommandLine: missing argument value message`` () =
    raisesWith<ArguParseException>
        <@ run<SimpleArgs> [| "--port" |] @>
        <| fun e -> <@ e.ErrorCode = ErrorCode.CommandLine
                     && e.FirstLine.StartsWith "ERROR: argument '--port' must be followed by" @>

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
    raisesWith<ArguParseException>
        <@ run<Level1Args> [| "level2"; "level3" |] @>
        <| fun e -> <@ e.ErrorCode = ErrorCode.PostProcess
                        // 'inner' is the deepest mandatory; its name should appear in the message.
                        && e.Message.Contains "--inner" @>

[<Fact>]
let ``Deep subcommand: providing the leaf mandatory parses cleanly`` () =
    let r = run<Level1Args> [| "level2"; "level3"; "--inner"; "ok" |]
    // Probe through the public surface: walk subcommand → subcommand → leaf.
    test <@ let l2 = r.GetResult Level2
            let l3 = l2.GetResult Level3
            l3.GetResult Inner = "ok" @>


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
    let parser = ArgumentParser.Create<AppSettingsArgs>()
    let dict = Dictionary<string, string>()
    dict["required-key"] <- "hello"
    dict["int-key"] <- "42"
    let reader = ConfigurationReader.FromDictionary dict
    let results = parser.ParseConfiguration(reader, ignoreMissing = false)
    test <@ results.GetResult(RequiredKey) = "hello" @>
    test <@ results.GetResult(IntKey) = 42 @>

[<Fact>]
let ``AppSettings: missing mandatory key raises`` () =
    let dict = Dictionary<string, string>()
    // required-key intentionally absent
    let reader = ConfigurationReader.FromDictionary dict
    let parser = ArgumentParser.Create<AppSettingsArgs>()
    raisesWith<ArguParseException>
        <@ parser.ParseConfiguration(reader, ignoreMissing = false) @>
        <| fun e -> <@ e.ErrorCode = ErrorCode.PostProcess
                        // Argu reports the CLI name in the missing-mandatory error, not the
                        // AppSettings key. The CLI name is auto-derived ("requiredkey").
                        && e.Message.Contains "--requiredkey" @>

[<Fact>]
let ``AppSettings: null or empty value is treated as absent`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>()
    let dict = readOnlyDict [ "required-key", "x"  // satisfy mandatory
                              "optional-key", "" ] // empty string should be treated as absent
    let reader = ConfigurationReader.FromDictionary dict
    let results = parser.ParseConfiguration(reader, ignoreMissing = false)
    test <@ results.TryGetResult(OptionalKey) = None @>

[<Fact>]
let ``AppSettings: invalid type-parse raises with key in message`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>()
    let dict = readOnlyDict [ "required-key", "x"
                              "int-key", "not-a-number" ]
    let reader = ConfigurationReader.FromDictionary dict

    raisesWith<ArguParseException>
        <@ parser.ParseConfiguration(reader, ignoreMissing = false) @>
        <| fun e -> <@ e.ErrorCode = ErrorCode.AppSettings
                        && e.Message.Contains "int-key" @>

[<Fact>]
let ``AppSettings: ignoreMissing=true skips mandatory check`` () =
    let parser = ArgumentParser.Create<AppSettingsArgs>()
    let reader = ConfigurationReader.FromDictionary(readOnlyDict [])
    // Should not raise.
    let results = parser.ParseConfiguration(reader, ignoreMissing = true)
    test <@ results.TryGetResult RequiredKey = None @>


type EnvArgs =
    | [<CustomAppSettings("ARGU_TEST_VAL")>] Val of string
    interface IArgParserTemplate with
        member this.Usage = "value"

[<Fact>]
let ``ParseConfiguration fallback to EnvironmentVariableConfigurationReader picks up process env`` () =
    use _ = EnvVarTests.envOverride "ARGU_TEST_VAL" "from-env"
    let parser = ArgumentParser.Create<EnvArgs>()
    let reader = ConfigurationReader.FromEnvironmentVariables()
    let results = parser.ParseConfiguration reader
    test <@ results.GetResult Val = "from-env" @>
