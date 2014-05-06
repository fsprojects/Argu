namespace Nessos.UnionArgParser

    module Tests =

        open System
        open System.IO

        open NUnit.Framework
        open FsUnit

        type Argument =
            | Working_Directory of string
            | Listener of string * int
            | [<Mandatory>] Mandatory_Arg of bool
            | [<Rest>] Rest_Arg of int
            | Log_Level of int
            | [<AltCommandLine("-D"); AltCommandLine("-z")>] Detach
            | [<CustomAppSettings("Foo")>] CustomAppConfig of string * int
            | [<First>] First_Parameter of string
        with
            interface IArgParserTemplate with
                member a.Usage =
                    match a with
                    | Working_Directory _ -> "specify a working directory."
                    | Listener _ -> "specify a listener (hostname : port)."
                    | Mandatory_Arg _ -> "a mandatory argument."
                    | Rest_Arg _ -> "an argument that consumes all remaining command line tokens."
                    | Log_Level _ -> "set the log level."
                    | Detach _ -> "detach daemon from console."
                    | CustomAppConfig _ -> "parameter with custom AppConfig key."
                    | First_Parameter _ -> "parameter that has to appear at beginning of command line args."

        let parser = new UnionArgParser<Argument>("usage string")

        [<Test>]
        let ``1. Simple command line parsing`` () =
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
        let ``2. Simple AppSettings parsing`` () =
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
        let ``3. Help String`` () =
            fun () -> parser.ParseCommandLine [| "--help" |] |> ignore
            |> should throw typeof<ArgumentException>

        [<Test>]
        let ``4. Missing Mandatory parameter`` () =
            fun () -> parser.ParseCommandLine [| "-D" |] |> ignore
            |> should throw typeof<ArgumentException>

        [<Test>]
        let ``5. First Parameter not placed at beggining`` () =
            fun () -> parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] |> ignore
            |> should throw typeof<ArgumentException>

        [<Test>]
        let ``6. Rest Parameter`` () =
            let args = [|1..100|] |> Array.map string |> Array.append [| "--mandatory-arg" ; "true" ; "--rest-arg" |]
            let result = parser.ParseCommandLine args
            result.GetResults <@ Rest_Arg @> |> should equal [1..100]

        [<Test>]
        let ``7. Multiple AltCommandLine`` () =
            let args = [| "--mandatory-arg" ; "true" ; "-z" |]
            let results = parser.ParseCommandLine args
            results.Contains <@ Detach @> |> should equal true