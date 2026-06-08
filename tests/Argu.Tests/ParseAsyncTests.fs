module Argu.Tests.ParseAsync

open Swensen.Unquote
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Xunit

open Argu

type Args =
    | TagKey of string
    | [<CustomAppSettings("port-key")>] PortKey of int
    interface IArgParserTemplate with member this.Usage = "x"

[<Fact>]
let ``ConfigurationReader.AsAsync wraps a sync reader`` () =
    let dict = readOnlyDict [ "x", "y" ]
    let sync = ConfigurationReader.FromDictionary dict
    let async = ConfigurationReader.AsAsync sync
    let keys : IReadOnlyCollection<string> = [| "x"; "missing" |] :> _
    let t = async.GetValuesAsync(keys)
    t.Wait()
    test <@ t.Result["x"] = "y" @>
    test <@ not (t.Result.ContainsKey "missing") @>
    test <@ async.Name = sync.Name @>

[<Fact>]
let ``ParseAsync via AsAsync(sync reader) matches sync Parse`` () =
    let parser = ArgumentParser.Create<Args>()
    let dict = readOnlyDict [ "tagkey", "release"; "port-key", "9090" ]
    let syncReader = ConfigurationReader.FromDictionary dict
    let asyncReader = ConfigurationReader.AsAsync syncReader

    let syncResults = parser.Parse(inputs = [||], configurationReader = syncReader)
    let asyncResults = parser.ParseAsync(inputs = [||], configurationReader = asyncReader).Result

    test <@ syncResults.GetResult(TagKey) = asyncResults.GetResult(TagKey) @>
    test <@ syncResults.GetResult(PortKey) = asyncResults.GetResult(PortKey) @>

[<Fact>]
let ``ParseAsync issues exactly one batched call covering all schema keys`` () =
    let parser = ArgumentParser.Create<Args>()
    let mutable calls = 0
    let mutable observedKeys : string [] = [||]
    let reader =
        { new IAsyncConfigurationReader with
            member _.Name = "counted-batched-reader"
            member _.GetValuesAsync(keys) =
                Interlocked.Increment(&calls) |> ignore
                observedKeys <- keys |> Seq.toArray
                Task.FromResult(readOnlyDict [ "tagkey", "alpha" ]) }
    let r = parser.ParseAsync(inputs = [||], configurationReader = reader).Result
    test <@ r.GetResult(TagKey) = "alpha" @>
    // Single round-trip regardless of key count.
    test <@ calls = 1 @>
    // Both schema-derived AppSettings keys land in the one batch.
    test <@ set observedKeys = set [| "port-key"; "tagkey" |] @>

[<Fact>]
let ``FromAsyncFunction adapts an F# async function`` () =
    let parser = ArgumentParser.Create<Args>()
    let asyncReader =
        ConfigurationReader.FromFunctionAsync(
            fun key -> task {
                return
                    if key = "tagkey" then Some "from-async"
                    else None })
    let r = parser.ParseAsync(inputs = [||], configurationReader = asyncReader).Result
    test <@ r.GetResult(TagKey) = "from-async" @>

[<Fact>]
let ``Bare faulted reader propagates exception out of ParseAsync`` () = task {
    let parser = ArgumentParser.Create<Args>()
    let reader =
        { new IAsyncConfigurationReader with
            member _.Name = "faulting-reader"
            member _.GetValuesAsync(_keys) =
                Task.FromException<IReadOnlyDictionary<string, string>>(
                    System.AggregateException "vault unavailable") }
    // No fallback wrapper - the fault is fatal, matching the documented contract.
    let! agg = Assert.ThrowsAsync<System.AggregateException>(fun () -> parser.ParseAsync(inputs = [||], configurationReader = reader))
    test <@ agg.Message = "vault unavailable" @> }

[<Fact>]
let ``WithFallback downgrades a faulted batch to substitute values`` () =
    let parser = ArgumentParser.Create<Args>()
    let inner =
        { new IAsyncConfigurationReader with
            member _.Name = "faulting-reader"
            member _.GetValuesAsync(_keys) =
                Task.FromException<IReadOnlyDictionary<string, string>>(
                    System.Exception "vault unavailable") }
    let mutable seenFault : exn option = None
    let reader =
        let handle ex =
            seenFault <- Some ex
            readOnlyDict [ "tagkey", "fallback" ]
            |> Task.FromResult
        ConfigurationReader.WithFallback(inner, fallback = handle)
    // Fault is swallowed; CLI args still satisfy the parse.
    let r = parser.ParseAsync(inputs = [||], configurationReader = reader).Result
    test <@ r.GetResult(TagKey) = "fallback" @>
    test <@ seenFault |> Option.exists (fun e -> e.Message = "vault unavailable") @>

[<Fact>]
let ``ParseAsync passes ignoreUnrecognized through`` () =
    let parser = ArgumentParser.Create<Args>()
    let reader = ConfigurationReader.AsAsync(ConfigurationReader.NullReader)
    let r =
        parser.ParseAsync(
            inputs = [| "--bogus" |],
            configurationReader = reader,
            ignoreUnrecognized = true,
            raiseOnUsage = false).Result
    test <@ r.UnrecognizedCliParams |> List.contains "--bogus" @>
