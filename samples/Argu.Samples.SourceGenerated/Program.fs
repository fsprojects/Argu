module Argu.Samples.SourceGenerated.Program

open Argu
open Argu.SourceGenerator

/// CLI template annotated with the source-generator marker. Today
/// nothing is generated; the marker is inert and Argu's reflection
/// path still computes the schema at runtime. When the generator
/// ships, this same template will gain a compile-time-built schema
/// and stop calling Activator.CreateInstance.
[<ArguGenerate>]
type SampleArgs =
    | [<Mandatory>] Port of int
    | Verbose
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Port _ -> "port to bind to"
            | Verbose -> "verbose output"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<SampleArgs>(programName = "demo")
    let results = parser.Parse(argv, ignoreMissing = true, raiseOnUsage = false)

    match results.TryGetResult(Port) with
    | Some p -> printfn "port: %d" p
    | None -> printfn "port: (unspecified)"

    if results.Contains(Verbose) then printfn "verbose: true"

    0
