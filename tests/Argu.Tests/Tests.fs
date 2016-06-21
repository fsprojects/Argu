namespace Argu.Tests

open System
open System.IO
open Xunit
open FsUnit.Xunit

open Argu

module ``Simple Tests`` =

    let shouldFailwith<'T, 'Exn when 'Exn :> exn>(f : unit -> 'T) =
        ignore <| Assert.Throws<'Exn>(f >> ignore)

    type Argument =
        | Working_Directory of string
        | [<PrintLabels>] Listener of host:string * port:int
        | [<Mandatory>] Mandatory_Arg of bool
        | [<Rest>] Rest_Arg of int
        | Data of int * byte []
        | Log_Level of int
        | [<AltCommandLine("/D", "-D", "-z")>] Detach
        | [<CustomAppSettings "Foo">] CustomAppConfig of string * int
        | [<EqualsAssignment>] Assignment of string
        | [<First>] First_Parameter of string
        | [<CliPrefix(CliPrefix.Dash)>] A
        | [<CliPrefix(CliPrefix.Dash)>] B
        | [<CliPrefix(CliPrefix.Dash)>] C
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
                | CustomAppConfig _ -> "parameter with custom AppConfig key."
                | First_Parameter _ -> "parameter that has to appear at beginning of command line args."
                | A | B | C -> "misc arguments"

    let parser = ArgumentParser.Create<Argument> "usage string"

    [<Fact>]
    let ``Simple command line parsing`` () =
        let args = 
            [| "--first-parameter" ; "bar" ; "--mandatory-arg" ; "true" ; "-D" ; 
                "--listener" ; "localhost" ; "8080" ; "--log-level" ; "2" |]

        let expected_outcome = set [ First_Parameter "bar" ; Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let results = parser.ParseCommandLine args
        results.GetAllResults() |> set |> should equal expected_outcome

        results.Contains <@ Detach @> |> should equal true
        results.GetResult <@ Listener @> |> should equal ("localhost", 8080)
        results.GetResults <@ Log_Level @> |> should equal [2]
        results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) |> should equal 3

    [<Fact>]
    let ``Simple AppSettings parsing`` () =
        let args = [ Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let xmlSource = parser.PrintAppSettings args
        let xmlFile = Path.GetTempFileName()
        do File.WriteAllText(xmlFile, xmlSource)
        let results = parser.ParseAppSettings(xmlFile)

        results.GetAllResults () |> set |> should equal (set args)

        results.Contains <@ Detach @> |> should equal true
        results.GetResult <@ Listener @> |> should equal ("localhost", 8080)
        results.GetResults <@ Log_Level @> |> should equal [2]
        results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) |> should equal 3

    [<Fact>]
    let ``Help String`` () =
        fun () -> parser.ParseCommandLine [| "--help" |] |> ignore
        |> should throw typeof<ArgumentException>

    [<Fact>]
    let ``Missing Mandatory parameter`` () =
        fun () -> parser.ParseCommandLine [| "-D" |] |> ignore
        |> should throw typeof<ArgumentException>

    [<Fact>]
    let ``First Parameter not placed at beggining`` () =
        fun () -> parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] |> ignore
        |> should throw typeof<ArgumentException>

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
        usage.Contains "<host:string>" |> should equal true
        usage.Contains "<port:int>" |> should equal true

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
    let ``Ignore Unrecognized parameters`` () =
        let args = 
            [| "--first-parameter" ; "bar" ; "--junk-param" ; "42" ; "--mandatory-arg" ; "true" ; "-D" ; 
                "--listener" ; "localhost" ; "8080" ; "--log-level" ; "2" |]

        let expected_outcome = set [ First_Parameter "bar" ; Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let results = parser.ParseCommandLine (args, ignoreUnrecognized = true)
        results.GetAllResults() |> set |> should equal expected_outcome

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

    type ConflictinAppSettingsNames =
        | [<CustomAppSettings "foo">]Foo of int
        | [<CustomAppSettings "foo">] Bar of string
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    [<Fact>]
    let ``Identify conflicting CLI identifiers`` () =
        shouldFailwith<_, ArgumentException> (fun () -> ArgumentParser.Create<ConflictingCliNames>())

    [<Fact>]
    let ``Identify conflicting AppSettings identifiers`` () =
        shouldFailwith<_, ArgumentException> (fun () -> ArgumentParser.Create<ConflictinAppSettingsNames>())


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



    [<CliPrefix(CliPrefix.Empty)>]
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
        shouldFailwith<_, ArgumentException>(fun () -> parser.ParseCommandLine [|"--assignment"; "value"|])

    [<Fact>]
    let ``Slash in Commandline`` () =
        let args = [| "--mandatory-arg" ; "true" ; "/D" |]
        let results = parser.ParseCommandLine args
        results.Contains <@ Detach @> |> should equal true
    
    [<Fact>]
    let ``Should fail wenn Usage, Mandatory and raiseOnUsage = true`` () =
        shouldFailwith<_, ArgumentException>(fun () -> parser.ParseCommandLine ([|"--help"|], raiseOnUsage = true))

    [<Fact>]
    let ``Usage, Mandatory and raiseOnusage = false`` () =
        let args = [| "--help" |]
        let results = parser.ParseCommandLine (args, raiseOnUsage = false)
        results.IsUsageRequested |> should equal true