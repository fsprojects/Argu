# Argu.SourceGenerator

Companion package for compile-time Argu schema generation. Right now it ships only the `[<ArguGenerate>]` marker attribute; the generator that consumes it will follow in a later release.

## Why

Argu's runtime schema computation uses reflection (`FSharpType.GetUnionCases`, `MakeGenericType` + `Activator.CreateInstance`). This is incompatible with publish-AOT, where `Activator.CreateInstance` over a runtime-built generic type cannot be JIT-compiled. The reflection path also slows startup for large CLI templates.

A source generator, given an annotated DU, can emit:

- A frozen `UnionArgInfo` record built from compile-time-known metadata
- A `Create`-equivalent factory that skips the reflection walk

Consumers opt in per template:

```fsharp
open Argu.SourceGenerator

[<ArguGenerate>]
type Args =
    | [<Mandatory>] Port of int
    | Verbose
    interface IArgParserTemplate with
        member this.Usage = ...
```

## Status

| Component | State |
|---|---|
| `[<ArguGenerate>]` marker | Shipped |
| Generator producing schema | Planned |
| Companion `ArgumentParser` factory consuming the schema | Planned |

## Why a separate package

Keeping the marker, the generator, and the runtime-bypass factory in a separate NuGet package preserves the core `Argu` package's zero-dependency footprint. Users that don't need AOT see no churn.

## Roadmap

1. Marker + scaffold (this PR)
2. F# source generator (Roslyn `FSharp.Compiler.Service` or external tool like Myriad) emitting a `precomputeSchemaFor<'T>()` function for the simplest schemas (no subcommands, no custom attributes)
3. Companion `ArgumentParser.CreateFromPrecomputed<'T>` overload that bypasses the reflection path
4. Expand generator coverage to subcommands, custom assignment attributes, inheritance and aliasing
