namespace Argu.Tests

open Xunit
open Swensen.Unquote

open Argu

/// Tests for the fluent ParseResults.Query API (PR 23).
module ``Argu Tests FluentQuery`` =

    type Args =
        | Port of int
        | Verbose
        | Tag of string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Port _ -> "port"
                | Verbose -> "verbose"
                | Tag _ -> "tag"

    let private parse (argv : string []) =
        ArgumentParser.Create<Args>(programName = "app")
            .ParseCommandLine(argv, raiseOnUsage = false)

    [<Fact>]
    let ``Query.Get returns the same as GetResult`` () =
        let r = parse [| "--port"; "8080" |]
        test <@ r.Query(<@ Port @>).Get() = r.GetResult(Port) @>

    [<Fact>]
    let ``Query.TryGet returns None when absent`` () =
        let r = parse [||]
        test <@ r.Query(<@ Tag @>).TryGet() = None @>

    [<Fact>]
    let ``Query.TryGet returns Some when present`` () =
        let r = parse [| "--tag"; "v1" |]
        test <@ r.Query(<@ Tag @>).TryGet() = Some "v1" @>

    [<Fact>]
    let ``Query.GetAll matches GetResults`` () =
        let r = parse [| "--tag"; "a"; "--tag"; "b"; "--tag"; "c" |]
        test <@ r.Query(<@ Tag @>).GetAll() = r.GetResults(Tag) @>
        test <@ r.Query(<@ Tag @>).GetAll() = [ "a"; "b"; "c" ] @>

    [<Fact>]
    let ``Query.GetOrDefault returns default when absent`` () =
        let r = parse [||]
        test <@ r.Query(<@ Port @>).GetOrDefault(8080) = 8080 @>

    [<Fact>]
    let ``Query.GetOrDefault returns value when present`` () =
        let r = parse [| "--port"; "1234" |]
        test <@ r.Query(<@ Port @>).GetOrDefault(8080) = 1234 @>

    [<Fact>]
    let ``Query.Count returns number of occurrences`` () =
        let r = parse [| "--tag"; "x"; "--tag"; "y" |]
        test <@ r.Query(<@ Tag @>).Count() = 2 @>

    [<Fact>]
    let ``Query.Exists tracks presence`` () =
        let r1 = parse [| "--port"; "1" |]
        let r2 = parse [||]
        // Query currently targets parameterised cases (Expr<'Field -> 'Template>);
        // nullary cases like Verbose use Contains directly.
        test <@ r1.Query(<@ Port @>).Exists() = true @>
        test <@ r2.Query(<@ Port @>).Exists() = false @>

    [<Fact>]
    let ``Query.FromCli matches the CommandLine source filter`` () =
        let r = parse [| "--port"; "1" |]
        // Result was sourced from CLI; FromCli must keep it.
        test <@ r.Query(<@ Port @>).FromCli().TryGet() = Some 1 @>

    [<Fact>]
    let ``Query.FromAppSettings excludes CommandLine-sourced results`` () =
        let r = parse [| "--port"; "1" |]
        // The result is from CommandLine; filtering to AppSettings must hide it.
        test <@ r.Query(<@ Port @>).FromAppSettings().TryGet() = None @>

    [<Fact>]
    let ``Query.PostProcess applies the parser`` () =
        let r = parse [| "--tag"; "release" |]
        let upper = r.Query(<@ Tag @>).PostProcess(fun s -> s.ToUpperInvariant()).Get()
        test <@ upper = "RELEASE" @>

    [<Fact>]
    let ``Query.PostProcess composes with FromCli`` () =
        let r = parse [| "--tag"; "abc" |]
        let n =
            r.Query(<@ Tag @>)
                .FromCli()
                .PostProcess(fun s -> s.Length)
                .Get()
        test <@ n = 3 @>

    [<Fact>]
    let ``Query.PostProcess.GetAll returns mapped list`` () =
        let r = parse [| "--tag"; "a"; "--tag"; "bb"; "--tag"; "ccc" |]
        let lens =
            r.Query(<@ Tag @>).PostProcess(fun s -> s.Length).GetAll()
        test <@ lens = [ 1; 2; 3 ] @>
