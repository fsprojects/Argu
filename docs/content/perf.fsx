(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Release/net45"
#r "Argu.dll"
#I "../../packages/xunit.extensibility.core/lib/netstandard1.1"
#r "xunit.core.dll"

open Argu
open Xunit

type Arguments =
    | Argument
with
    interface IArgParserTemplate with
        member __.Usage =
            "Usage"

(**

# Performance Tips

## Introduction

Argu simplicity is achieved via Reflection and as such it's performance heavily depend on the size and depth of the
discriminated union used.

For applications that wants to get a little more performance out of Argu it's also possible to get a little more
performance.

## Bypassing structure checks

By default Argu checks that the discriminated union is well formed and only contains entries that are valid.
This incur both the cost of the checks themselves but also the cost of materializing the whole argument graph that could
be loaded only if the corresponding arguments are used.

This check can easilly be bypassed either only in release builds :

*)

let checkStructure =
#if DEBUG
    true
#else
    false
#endif

let parser = ArgumentParser.Create<Arguments>(checkStructure = checkStructure)

(**

Or always, forcing the check to happen during unit tests:

*)

// In the application
module AppArgs =
    let parser = ArgumentParser.Create<Arguments>(checkStructure = false)

// In tests
[<Fact>]
let ``Argument structure is correct`` () =
    ArgumentParser<Arguments>.CheckStructure()
(**
*)
