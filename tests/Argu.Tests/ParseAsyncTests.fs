namespace Argu.Tests

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Xunit
open Swensen.Unquote

open Argu

/// Tests for IAsyncConfigurationReader + ParseAsync (PR 29).
module ``Argu Tests ParseAsync`` =

    type Args =
        | TagKey of string
        | [<CustomAppSettings("port-key")>] PortKey of int
        interface IArgParserTemplate with member this.Usage = "x"

    [<Fact>]
    let ``ConfigurationReader.AsAsync wraps a sync reader`` () =
        let dict = Dictionary<string, string>()
        dict["x"] <- "y"
        let sync = ConfigurationReader.FromDictionary dict
        let async = ConfigurationReader.AsAsync sync
        let t = async.GetValueAsync("x")
        t.Wait()
        test <@ t.Result = "y" @>
        test <@ async.Name = sync.Name @>

    [<Fact>]
    let ``ParseAsync via AsAsync(sync reader) matches sync Parse`` () =
        let parser = ArgumentParser.Create<Args>(programName = "app")
        let dict = Dictionary<string, string>()
        dict["tagkey"] <- "release"
        dict["port-key"] <- "9090"
        let syncReader = ConfigurationReader.FromDictionary dict
        let asyncReader = ConfigurationReader.AsAsync syncReader

        let syncResults = parser.Parse(inputs = [||], configurationReader = syncReader)
        let asyncResults =
            parser.ParseAsync(inputs = [||], configurationReader = asyncReader).Result

        test <@ syncResults.GetResult(TagKey) = asyncResults.GetResult(TagKey) @>
        test <@ syncResults.GetResult(PortKey) = asyncResults.GetResult(PortKey) @>

    [<Fact>]
    let ``ParseAsync pre-fetches each schema key exactly once`` () =
        let parser = ArgumentParser.Create<Args>(programName = "app")
        let mutable lookups = 0
        let reader =
            { new IAsyncConfigurationReader with
                member _.Name = "counted-async-reader"
                member _.GetValueAsync(key) =
                    Interlocked.Increment(&lookups) |> ignore
                    let v = if key = "tagkey" then "alpha" else null
                    Task.FromResult v }
        let r = parser.ParseAsync(inputs = [||], configurationReader = reader).Result
        test <@ r.GetResult(TagKey) = "alpha" @>
        // Schema has two AppSettings keys (tagkey, port-key); each should be
        // fetched once.
        test <@ lookups = 2 @>

    [<Fact>]
    let ``FromAsyncFunction adapts an F# async function`` () =
        let parser = ArgumentParser.Create<Args>(programName = "app")
        let asyncReader =
            ConfigurationReader.FromAsyncFunction(
                fun key ->
                    async {
                        return
                            if key = "tagkey" then Some "from-async"
                            else None
                    })
        let r = parser.ParseAsync(inputs = [||], configurationReader = asyncReader).Result
        test <@ r.GetResult(TagKey) = "from-async" @>

    [<Fact>]
    let ``ParseAsync treats faulted GetValueAsync as missing`` () =
        let parser = ArgumentParser.Create<Args>(programName = "app")
        let reader =
            { new IAsyncConfigurationReader with
                member _.Name = "faulting-reader"
                member _.GetValueAsync(_key) =
                    Task.FromException<string>(System.Exception "vault unavailable") }
        // Faulted reader must not throw; CLI args still parsed normally
        let r = parser.ParseAsync(inputs = [| "--tagkey"; "v1" |], configurationReader = reader).Result
        test <@ r.GetResult(TagKey) = "v1" @>

    [<Fact>]
    let ``ParseAsync passes ignoreUnrecognized through`` () =
        let parser = ArgumentParser.Create<Args>(programName = "app")
        let reader = ConfigurationReader.AsAsync(ConfigurationReader.NullReader)
        let r =
            parser.ParseAsync(
                inputs = [| "--bogus" |],
                configurationReader = reader,
                ignoreUnrecognized = true,
                raiseOnUsage = false).Result
        test <@ r.UnrecognizedCliParams |> List.contains "--bogus" @>