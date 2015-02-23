namespace Nessos.UnionArgParser

    module Tests =

        open System
        open System.IO

        open NUnit.Framework
        open FsUnit

        type Record = { Name : string ; Age : int }

        type Argument =
            | Working_Directory of string
            | [<PrintLabels>] Listener of host:string * port:int
            | [<Mandatory>] Mandatory_Arg of bool
            | [<Rest>] Rest_Arg of int
            | Data of int * byte []
            | Log_Level of int
            | [<AltCommandLine("-D"); AltCommandLine("-z")>] Detach
            | [<CustomAppSettings("Foo")>] CustomAppConfig of string * int
            | [<EqualsAssignment>] Assignment of string
            | [<First>] First_Parameter of string
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

        let parser = UnionArgParser.Create<Argument>("usage string")

        [<Test>]
        let ``01. Simple command line parsing`` () =
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

        [<Test>]
        let ``02. Simple AppSettings parsing`` () =
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

        [<Test>]
        let ``03. Help String`` () =
            fun () -> parser.ParseCommandLine [| "--help" |] |> ignore
            |> should throw typeof<ArgumentException>

        [<Test>]
        let ``04. Missing Mandatory parameter`` () =
            fun () -> parser.ParseCommandLine [| "-D" |] |> ignore
            |> should throw typeof<ArgumentException>

        [<Test>]
        let ``05. First Parameter not placed at beggining`` () =
            fun () -> parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] |> ignore
            |> should throw typeof<ArgumentException>

        [<Test>]
        let ``06. Rest Parameter`` () =
            let args = [|1..100|] |> Array.map string |> Array.append [| "--mandatory-arg" ; "true" ; "--rest-arg" |]
            let result = parser.ParseCommandLine args
            result.GetResults <@ Rest_Arg @> |> should equal [1..100]

        [<Test>]
        let ``07. Multiple AltCommandLine`` () =
            let args = [| "--mandatory-arg" ; "true" ; "-z" |]
            let results = parser.ParseCommandLine args
            results.Contains <@ Detach @> |> should equal true

        [<Test>]
        let ``08. Usage documents explicitly named argument union case values`` () =
            let usage = parser.Usage()
            usage |> should contain "<host:string>"
            usage |> should contain "<port:int>"

        [<Test>]
        let ``09. Parse byte[] parameters`` () =
            let bytes = [|1uy .. 255uy|]
            let args = parser.PrintCommandLine [ Mandatory_Arg false ; Data(42, bytes) ]
            let results = parser.ParseCommandLine args
            results.GetResult <@ Data @> |> snd |> should equal bytes

        [<Test>]
        let ``10. Parse equals assignment`` () =
            let arg = [ Assignment "foo bar" ]
            let clp = parser.PrintCommandLine arg
            let result = parser.Parse clp
            result.GetResult <@ Assignment @> |> should equal "foo bar"


        [<Test>]
        let ``11. Ignore Unrecognized parameters`` () =
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


        [<Test>]
        let ``12. Should allow '--help' before first args`` () =
            let result = parser.Parse([| "--help" ; "--first-parameter" ; "bar"|], raiseOnUsage = false)
            result.IsUsageRequested |> should equal true

        [<Test>]
        let ``13. Should allow '--help' before mandatory args`` () =
            let result = parser.Parse([| "--help" ; "--mandatory-arg" ; "true"|], raiseOnUsage = false)
            result.IsUsageRequested |> should equal true

        type ConflictingCliNames =
            | [<CustomCommandLine("foo")>] Foo of int
            | [<AltCommandLine("foo")>] Bar of string
        with
            interface IArgParserTemplate with
                member a.Usage = "foo"

        type ConflictinAppSettingsNames =
            | [<CustomAppSettings("foo")>]Foo of int
            | [<CustomAppSettings("foo")>] Bar of string
        with
            interface IArgParserTemplate with
                member a.Usage = "foo"

        [<Test; ExpectedException(typeof<FormatException>)>]
        let ``14. Identify conflicting CLI identifiers`` () =
            ignore <| UnionArgParser.Create<ConflictingCliNames>("usage string")

        [<Test; ExpectedException(typeof<FormatException>)>]
        let ``15. Identify conflicting AppSettings identifiers`` () =
            ignore <| UnionArgParser.Create<ConflictinAppSettingsNames>("usage string")


        [<CliPrefix(CliPrefix.Dash)>]
        type ArgumentSingleDash =
            | Argument of string
            | Levels_Deep of int
        with
            interface IArgParserTemplate with
                member a.Usage = "not tested here"


        [<Test>]
        let ``16. Use single dash prefix as default`` () =
            let parser = UnionArgParser.Create<ArgumentSingleDash>("usage string")
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


        [<Test>]
        let ``17. Use no prefix as default`` () =
            let parser = UnionArgParser.Create<ArgumentNoDash>("usage string")
            let args = 
                [| "argument" ; "bar" ; "levels-deep" ; "3" |]

            let expected_outcome = set [ Argument "bar" ; Levels_Deep 3 ]
            let results = parser.ParseCommandLine args
            results.GetAllResults() |> set |> should equal expected_outcome

            results.Contains <@ Argument @> |> should equal true
            results.GetResult <@ Levels_Deep @> |> should equal 3