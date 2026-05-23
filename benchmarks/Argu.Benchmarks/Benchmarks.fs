namespace Argu.Benchmarks

open BenchmarkDotNet.Attributes

open Argu

/// Schema used by all benchmarks. Deliberately small but covers
/// optional, list and primitive parameter shapes.
[<AutoOpen>]
module Schema =

    type Args =
        | [<Mandatory>] Port of int
        | [<AltCommandLine("-v")>] Verbose
        | Tag of string
        | Items of int list
        | Working_Directory of string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Port _ -> "port to bind to"
                | Verbose -> "verbose output"
                | Tag _ -> "an opaque tag"
                | Items _ -> "list of int items"
                | Working_Directory _ -> "working directory"

/// Schema build benchmark — measures the cost of constructing the
/// reflection-based parser for the schema above.
[<MemoryDiagnoser>]
type SchemaBuild () =

    [<Benchmark>]
    member _.Create () =
        ArgumentParser.Create<Args>(programName = "bench")

/// Parse benchmark — schema is built once at class init and the same
/// parser is reused for every iteration.
[<MemoryDiagnoser>]
type Parse () =
    let parser = ArgumentParser.Create<Args>(programName = "bench")
    let argv = [| "--port"; "8080"; "-v"; "--tag"; "release"; "--working-directory"; "/tmp" |]

    [<Benchmark>]
    member _.ParseCommandLine () =
        parser.ParseCommandLine(argv, raiseOnUsage = false)

/// Help render benchmark — the parser is built once; we re-render the
/// usage string on every iteration.
[<MemoryDiagnoser>]
type HelpRender () =
    let parser = ArgumentParser.Create<Args>(programName = "bench")

    [<Benchmark>]
    member _.PrintUsage () =
        parser.PrintUsage()
