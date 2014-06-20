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
            | [<EncodeBase64>] Data of byte []
            | [<EncodeBase64>] Record of Record
            | Log_Level of int
            | [<AltCommandLine("-D"); AltCommandLine("-z")>] Detach
            | [<CustomAppSettings("Foo")>] CustomAppConfig of string * int
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
                    | Record _ -> "pass an F# record in base64 format."
                    | Log_Level _ -> "set the log level."
                    | Detach _ -> "detach daemon from console."
                    | CustomAppConfig _ -> "parameter with custom AppConfig key."
                    | First_Parameter _ -> "parameter that has to appear at beginning of command line args."

        let parser = new UnionArgParser<Argument>("usage string")

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
            let args = parser.PrintCommandLine [ Mandatory_Arg false ; Data bytes ]
            let results = parser.ParseCommandLine args
            results.GetResult <@ Data @> |> should equal bytes


        [<Test>]
        let ``10. Parse binary serialized value`` () =
            let instance = { Name = "me" ; Age = -1 }
            let args = parser.PrintCommandLine [ Mandatory_Arg false ; Record instance ]
            let results = parser.ParseCommandLine args
            results.GetResult <@ Record @> |> should equal instance