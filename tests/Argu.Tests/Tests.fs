namespace Argu.Tests

open System
open System.IO
open Xunit
open FsUnit.Xunit

open Argu

module ``Simple Tests`` =

    let shouldFailwith<'T, 'Exn when 'Exn :> exn>(f : unit -> 'T) =
        ignore <| Assert.Throws<'Exn>(f >> ignore)

    type PushArgs =
        | Remote of name:string
        | Branch of name:string
    with
        interface IArgParserTemplate with
            member this.Usage = "push"

    [<CliPrefix(CliPrefix.Dash)>]
    type CleanArgs =
        | D
        | F
        | X
    with
        interface IArgParserTemplate with
            member this.Usage = "clean"

    type Argument =
        | Working_Directory of string
        | Listener of host:string * port:int
        | [<Mandatory>] Mandatory_Arg of bool
        | [<Rest>] Rest_Arg of int
        | Data of int * byte []
        | Log_Level of int
        | [<AltCommandLine("/D", "-D", "-z")>] Detach
        | [<CustomAppSettings "Foo">] CustomAppConfig of string * int
        | [<EqualsAssignment>] Assignment of string
        | [<EqualsAssignment>] Env of key:string * value:string
        | [<First>] First_Parameter of string
        | [<CliPrefix(CliPrefix.Dash)>] A
        | [<CliPrefix(CliPrefix.Dash)>] B
        | [<CliPrefix(CliPrefix.Dash)>] C
        | [<CliPrefix(CliPrefix.None)>] Push of ParseResult<PushArgs>
        | [<CliPrefix(CliPrefix.None)>] Clean of ParseResult<CleanArgs>
    with
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Working_Directory _ -> "specify a working directory."
                | Listener _ -> "specify a listener."
                | Mandatory_Arg _ -> "a mandatory argument."
                | Rest_Arg _ -> "an argument that consumes all remaining command line tokens."
                | Data _ -> "pass raw data in base64 format."
                | Log_Level _ -> "set the log level."
                | Detach _ -> "detach daemon from console."
                | Assignment _ -> "assign with equals operation."
                | Env _ -> "assign environment variables."
                | CustomAppConfig _ -> "parameter with custom AppConfig key."
                | First_Parameter _ -> "parameter that has to appear at beginning of command line args."
                | Push _ -> "push changes"
                | Clean _ -> "clean state"
                | A | B | C -> "misc arguments"

    let parser = ArgumentParser.Create<Argument> ()

    [<Fact>]
    let ``Simple command line parsing`` () =
        let args = 
            [| "--first-parameter" ; "bar" ; "--mandatory-arg" ; "true" ; "-D" ; 
                "--listener" ; "localhost" ; "8080" ; "--log-level" ; "2" |]

        let expected_outcome = [ First_Parameter "bar" ; Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let results = parser.ParseCommandLine args
        results.GetAllResults() |> should equal expected_outcome

        results.Contains <@ Detach @> |> should equal true
        results.GetResult <@ Listener @> |> should equal ("localhost", 8080)
        results.GetResults <@ Log_Level @> |> should equal [2]
        results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) |> should equal 3

    [<Fact>]
    let ``Simple AppSettings parsing`` () =
        let args = [ Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ] |> List.sortBy tagOf
        let xmlSource = parser.PrintAppSettings args
        let xmlFile = Path.GetTempFileName()
        do File.WriteAllText(xmlFile, xmlSource)
        let results = parser.ParseAppSettings(xmlFile)

        results.GetAllResults () |> should equal args

        results.Contains <@ Detach @> |> should equal true
        results.GetResult <@ Listener @> |> should equal ("localhost", 8080)
        results.GetResults <@ Log_Level @> |> should equal [2]
        results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) |> should equal 3

    [<Fact>]
    let ``Help String`` () =
        fun () -> parser.ParseCommandLine [| "--help" |] |> ignore
        |> shouldFailwith<_, ArguParseException>

    [<Fact>]
    let ``Missing Mandatory parameter`` () =
        fun () -> parser.ParseCommandLine [| "-D" |] |> ignore
        |> shouldFailwith<_, ArguParseException>

    [<Fact>]
    let ``First Parameter not placed at beggining`` () =
        fun () -> parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] |> ignore
        |> shouldFailwith<_, ArguParseException>

    [<Fact>]
    let ``Rest Parameter`` () =
        let args = [|1..100|] |> Array.map string |> Array.append [| "--mandatory-arg" ; "true" ; "--rest-arg" |]
        let result = parser.ParseCommandLine args
        result.GetResults <@ Rest_Arg @> |> should equal [1..100]

    [<Fact>]
    let ``Multiple AltCommandLine`` () =
        let args = [| "--mandatory-arg" ; "true" ; "-z" |]
        let results = parser.ParseCommandLine args
        results.Contains <@ Detach @> |> should equal true

    [<Fact>]
    let ``Usage documents explicitly named argument union case values`` () =
        let usage = parser.Usage()
        usage.Contains "<host>" |> should equal true
        usage.Contains "<port>" |> should equal true

    [<Fact>]
    let ``Parse byte[] parameters`` () =
        let bytes = [|1uy .. 255uy|]
        let args = parser.PrintCommandLine [ Mandatory_Arg false ; Data(42, bytes) ]
        let results = parser.ParseCommandLine args
        results.GetResult <@ Data @> |> snd |> should equal bytes

    [<Fact>]
    let ``Parse equals assignment`` () =
        let arg = [ Assignment "foo bar" ]
        let clp = parser.PrintCommandLine arg
        let result = parser.Parse(clp, ignoreMissing = true)
        result.GetResult <@ Assignment @> |> should equal "foo bar"

    [<Fact>]
    let ``Parse key-value equals assignment`` () =
        let arg = [ Env("foo", "bar") ]
        let clp = parser.PrintCommandLine arg
        let result = parser.Parse(clp, ignoreMissing = true)
        result.GetResult <@ Env @> |> should equal ("foo", "bar")


    [<Fact>]
    let ``Ignore Unrecognized parameters`` () =
        let args = 
            [| "--first-parameter" ; "bar" ; "--junk-param" ; "42" ; "--mandatory-arg" ; "true" ; "-D" ; 
                "--listener" ; "localhost" ; "8080" ; "--log-level" ; "2" |]

        let expected_outcome = [ First_Parameter "bar" ; Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let results = parser.ParseCommandLine (args, ignoreUnrecognized = true)
        results.GetAllResults() |> should equal expected_outcome

        results.Contains <@ Detach @> |> should equal true
        results.GetResult <@ Listener @> |> should equal ("localhost", 8080)
        results.GetResults <@ Log_Level @> |> should equal [2]
        results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) |> should equal 3


    [<Fact>]
    let ``Should recognize grouped switches`` () =
        let args = [| "-cba"; "-cc" |]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        results.GetAllResults() |> should equal [C; B; A; C; C]

    [<Fact>]
    let ``Simple subcommand parsing 1`` () =
        let args = [|"push"; "--remote" ; "origin" ; "--branch" ; "master"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Push @>
        nested.GetAllResults() |> should equal [Remote "origin" ; Branch "master"]

    [<Fact>]
    let ``Simple subcommand parsing 2`` () =
        let args = [|"clean"; "-fdx"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Clean @>
        nested.GetAllResults() |> should equal [F; D; X]

    [<Fact>]
    let ``Unrecognized CLI params`` () =
        let args = [| "--mandatory-arg" ; "true" ; "foobar" ; "-z" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        results.UnrecognizedCliParams |> should equal ["foobar"]
        results.Contains <@ Detach @> |> should equal true

    [<Fact>]
    let ``Nested unrecognized CLI params`` () =
        let args = [| "push" ; "foobar" ; "--branch" ; "master" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true, ignoreMissing = true)
        let nested = results.GetResult <@ Push @>
        nested.UnrecognizedCliParams |> should equal ["foobar"]
        nested.Contains <@ Branch @> |> should equal true
        results.UnrecognizedCliParams = [] |> should equal true // 'should equal []' fails here for whatever reason

    [<Fact>]
    let ``Should allow '--help' before first args`` () =
        let result = parser.Parse([| "--help" ; "--first-parameter" ; "bar"|], raiseOnUsage = false, ignoreMissing = true)
        result.IsUsageRequested |> should equal true

    [<Fact>]
    let ``Should allow '--help' before mandatory args`` () =
        let result = parser.Parse([| "--help" ; "--mandatory-arg" ; "true"|], raiseOnUsage = false)
        result.IsUsageRequested |> should equal true

    type ConflictingCliNames =
        | [<CustomCommandLine "foo">] Foo of int
        | [<AltCommandLine "foo">] Bar of string
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    type ConflictingAppSettingsNames =
        | [<CustomAppSettings "foo">]Foo of int
        | [<CustomAppSettings "foo">] Bar of string
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    type RecursiveArgument1 =
        | Rec1 of ParseResult<RecursiveArgument2>
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    and RecursiveArgument2 =
        | Rec2 of ParseResult<RecursiveArgument3>
    with
        interface IArgParserTemplate with
            member a.Usage = "bar"

    and RecursiveArgument3 =
        | Rec3 of ParseResult<RecursiveArgument1>
    with
        interface IArgParserTemplate with
            member a.Usage = "baz"

    type GenericArgument<'T> =
        | Bar of 'T
    with
        interface IArgParserTemplate with
            member a.Usage = "baz"

    [<Fact>]
    let ``Identify conflicting CLI identifiers`` () =
        shouldFailwith<_, ArguException> (fun () -> ArgumentParser.Create<ConflictingCliNames>())

    [<Fact>]
    let ``Identify conflicting AppSettings identifiers`` () =
        shouldFailwith<_, ArguException> (fun () -> ArgumentParser.Create<ConflictingAppSettingsNames>())

    [<Fact>]
    let ``Identify recursive subcommands`` () =
        shouldFailwith<_, ArguException> (fun () -> ArgumentParser.Create<RecursiveArgument1>())

    [<Fact>]
    let ``Identify generic arguments`` () =
        shouldFailwith<_, ArguException> (fun () -> ArgumentParser.Create<GenericArgument<string>>())


    [<CliPrefix(CliPrefix.Dash)>]
    type ArgumentSingleDash =
        | Argument of string
        | Levels_Deep of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"


    [<Fact>]
    let ``Use single dash prefix as default`` () =
        let parser = ArgumentParser.Create<ArgumentSingleDash>("usage string")
        let args = 
            [| "-argument" ; "bar" ; "-levels-deep" ; "3" |]

        let expected_outcome = set [ Argument "bar" ; Levels_Deep 3 ]
        let results = parser.ParseCommandLine args
        results.GetAllResults() |> set |> should equal expected_outcome

        results.Contains <@ Argument @> |> should equal true
        results.GetResult <@ Levels_Deep @> |> should equal 3



    [<CliPrefix(CliPrefix.None)>]
    type ArgumentNoDash =
        | Argument of string
        | Levels_Deep of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"


    [<Fact>]
    let ``Use no prefix as default`` () =
        let parser = ArgumentParser.Create<ArgumentNoDash>("usage string")
        let args = 
            [| "argument" ; "bar" ; "levels-deep" ; "3" |]

        let expected_outcome = set [ Argument "bar" ; Levels_Deep 3 ]
        let results = parser.ParseCommandLine args
        results.GetAllResults() |> set |> should equal expected_outcome

        results.Contains <@ Argument @> |> should equal true
        results.GetResult <@ Levels_Deep @> |> should equal 3

    [<Fact>]
    let ``Should fail if EqualsAssignment missing assignment.`` () =
        shouldFailwith<_, ArguParseException>(fun () -> parser.ParseCommandLine [|"--assignment"; "value"|])

    [<Fact>]
    let ``Slash in Commandline`` () =
        let args = [| "--mandatory-arg" ; "true" ; "/D" |]
        let results = parser.ParseCommandLine args
        results.Contains <@ Detach @> |> should equal true
    
    [<Fact>]
    let ``Should fail wenn Usage, Mandatory and raiseOnUsage = true`` () =
        shouldFailwith<_, ArguParseException>(fun () -> parser.ParseCommandLine ([|"--help"|], raiseOnUsage = true))

    [<Fact>]
    let ``Usage, Mandatory and raiseOnusage = false`` () =
        let args = [| "--help" |]
        let results = parser.ParseCommandLine (args, raiseOnUsage = false)
        results.IsUsageRequested |> should equal true


    [<HelpFlags("--my-help")>]
    [<HelpDescription("waka jawaka")>]
    type ArgumentWithAltHelp =
        | AltHelp1 of string
        | AltHelp2 of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<DisableHelpFlags>]
    type NoHelp =
        | NoHelp1 of string
        | NoHelp2 of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<Fact>]
    let ``Custom Help attribute`` () =
        let parser = ArgumentParser.Create<ArgumentWithAltHelp>()
        let results = parser.Parse([|"--my-help"|], raiseOnUsage = false)
        results.IsUsageRequested |> should equal true

    [<Fact>]
    let ``Custom Help Description attribute`` () =
        let parser = ArgumentParser.Create<ArgumentWithAltHelp>()
        parser.Usage().Contains "waka jawaka" |> should equal true

    [<Fact>]
    let ``Custom Help attribute should not use default helper`` () =
        let parser = ArgumentParser.Create<ArgumentWithAltHelp>()
        let results = parser.Parse([|"--help"|], raiseOnUsage = false, ignoreUnrecognized = true)
        results.IsUsageRequested |> should equal false

    [<Fact>]
    let ``Disable Help attribute`` () =
        let parser = ArgumentParser.Create<NoHelp>()
        let result = parser.Parse([|"--help"|], raiseOnUsage = false, ignoreUnrecognized = true)
        result.IsUsageRequested |> should equal false