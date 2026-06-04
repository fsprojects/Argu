module Argu.Tests.ParseConfigTests

open System.Collections.Generic
open Swensen.Unquote
open Xunit

open Argu

type Args =
    | [<Mandatory>] Port of int
    | Verbose
    | Tag of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Port _ -> "port"
            | Verbose -> "verbose"
            | Tag _ -> "tag"

let private parser () = ArgumentParser.Create<Args>()
[<Fact>]
let ``ParseConfig.Default holds historical defaults`` () =
    let d = ParseConfig.Default
    test <@ d.Inputs = None @>
    test <@ d.ConfigurationReader = None @>
    test <@ d.IgnoreMissing = false @>
    test <@ d.IgnoreUnrecognized = false @>
    test <@ d.RaiseOnUsage = true @>

[<Fact>]
let ``Parse(config with explicit inputs) parses those inputs`` () =
    let p = parser ()
    let argv = [| "--port"; "8080"; "--verbose" |]
    let cfg = { ParseConfig.Default with Inputs = Some argv ; RaiseOnUsage = false }
    let results = p.Parse(cfg)
    test <@ results.GetResult(Port) = 8080 @>
    test <@ results.Contains(Verbose) @>

[<Fact>]
let ``Parse(config) matches Parse(?inputs, ...) for the same parameters`` () =
    let p = parser ()
    let argv = [| "--port"; "1234"; "--tag"; "v1" |]
    let viaConfig =
        let cfg = { ParseConfig.Default with Inputs = Some argv ; RaiseOnUsage = false }
        p.Parse(cfg)
    let viaOptional = p.Parse(inputs = argv, raiseOnUsage = false)
    test <@ viaConfig.GetResult(Port) = viaOptional.GetResult(Port) @>
    test <@ viaConfig.GetResult(Tag) = viaOptional.GetResult(Tag) @>

[<Fact>]
let ``Parse(config with IgnoreMissing=true) skips mandatory check`` () =
    let p = parser ()
    let cfg = { ParseConfig.Default with Inputs = Some [||] ; IgnoreMissing = true }
    let results = p.Parse(cfg)
    test <@ results.TryGetResult(Port) = None @>

[<Fact>]
let ``Parse(config with IgnoreUnrecognized=true) collects unknown args`` () =
    let p = parser ()
    let cfg =
        { ParseConfig.Default with
            Inputs = Some [| "--port"; "1"; "--bogus" |]
            IgnoreUnrecognized = true
            RaiseOnUsage = false }
    let results = p.Parse(cfg)
    test <@ results.UnrecognizedCliParams |> List.contains "--bogus" @>

/// Argu's missing-mandatory check fires from the CLI even when AppSettings provides a value (pre-existing behavior),
/// so the AppSettings round-trip test uses a non-mandatory schema.
type AppSettingsArgs =
    | TagKey of string
    interface IArgParserTemplate with member this.Usage = "tag"

[<Fact>]
let ``Parse(config with ConfigurationReader) sources AppSettings`` () =
    let p = ArgumentParser.Create<AppSettingsArgs>()
    let dict = Dictionary<string, string>()
    dict["tagkey"] <- "v1"
    let reader = ConfigurationReader.FromDictionary dict
    let cfg =
        { ParseConfig.Default with
            Inputs = Some [||]
            ConfigurationReader = Some reader }
    let results = p.Parse(cfg)
    test <@ results.GetResult(TagKey) = "v1" @>
