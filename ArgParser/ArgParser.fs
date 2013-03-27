namespace UnionArgParser

    open System
    open System.Configuration
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    open UnionArgParser.Utils
    open UnionArgParser.InternalImpls




    type UnionArgParser<'Template when 'Template :> IArgParserTemplate> (?usageText : string) =
        do 
            if not <| FSharpType.IsUnion typeof<'Template> then
                failwith "UnionArgParser: template type must be F# DU."

        let argInfo =
            typeof<'Template>
            |> FSharpType.GetUnionCases
            |> Seq.map preComputeArgInfo
            |> Seq.sortBy (fun a -> a.UCI.Tag)
            |> Seq.toList

        let clArgIdx =
            argInfo
            |> Seq.map (fun aI -> aI.CommandLineNames |> Seq.map (fun name -> name, aI))
            |> Seq.concat
            |> Map.ofSeq

        let (|ParserExn|_|) (e : exn) =
            match e with
            | Bad (id, msg) -> Some (id, usage (Some msg) argInfo)
            | HelpText -> Some (ErrorCode.HelpText, usage usageText argInfo)
            | _ -> None

        let parseCommandLinePartial (args : string []) (pos : int ref) (state : Map<ArgId, ParseResult<'Template> list>) =
            let curr = args.[!pos]
            do incr pos
            
            if hasCommandLineParam helpInfo curr then raise HelpText

            match clArgIdx.TryFind curr with
            | None -> bad ErrorCode.CommandLine "unrecognized argument: '%s'." curr
            | Some argInfo ->
                let parseOne () =
                    let fields =
                        [|
                            for p in argInfo.Parsers do
                                if !pos = args.Length then
                                    bad ErrorCode.CommandLine "option '%s' requires argument <%s>." curr p.Name
                                yield 
                                    try p.Parser args.[!pos]
                                    with _ -> 
                                        bad ErrorCode.CommandLine "option '%s' expects argument <%s>." curr p.Name
                                incr pos
                        |]

                    buildResult argInfo curr fields

                let parsedResults =
                    if argInfo.Rest then
                        [
                            while !pos < args.Length do
                                yield parseOne ()
                        ]
                    else [ parseOne () ]

                let previous = defaultArg (state.TryFind argInfo.Id) []
                
                state.Add(argInfo.Id, previous @ parsedResults)
                

        let parseCommandLine (inputs : string []) =
            let state = ref Map.empty
            let pos = ref 0

            while !pos < inputs.Length do
                state := parseCommandLinePartial inputs pos !state

            !state

        // AppSettings parse errors are threaded to the state rather than raised directly;
        // this happens since AppSettings errors are overriden by default in case of a valid command line input.
        let parseAppSettingsPartial (state : Map<ArgId, Choice<ParseResult<'Template> list, exn>>) (aI : ArgInfo) =
            try
                match aI.AppSettingsName with
                | None -> state
                | Some name ->
                    let parseResults =
                        match ConfigurationManager.AppSettings.[name] with
                        | null | "" -> []
                        | entry when aI.Parsers.Length = 0 ->
                            match Boolean.tryParse entry with
                            | None -> bad ErrorCode.AppSettings "AppSettings entry '%s' is not <bool>." name
                            | Some flag when flag -> [buildResult aI name [||]]
                            | Some _ -> []
                        | entry ->
                            let tokens = if aI.AppSettingsCSV || aI.Parsers.Length > 1 then entry.Split(',') else [| entry |]
                            let pos = ref 0

                            let readNext() =
                                let fields =
                                    [|
                                        for p in aI.Parsers do
                                            if !pos < tokens.Length then
                                                yield 
                                                    try p.Parser <| tokens.[!pos]
                                                    with _ -> bad ErrorCode.AppSettings "AppSettings entry '%s' is not <%s>." name p.Name

                                                incr pos
                                            else
                                                bad ErrorCode.AppSettings "AppSettings entry '%s' misses <%s> argument." name p.Name
                                    |]

                                buildResult aI name fields

                            if aI.AppSettingsCSV then
                                [
                                    while !pos < tokens.Length do
                                        yield readNext ()
                                ]
                            else [ readNext () ]

                    state.Add(aI.Id, Choice1Of2 parseResults)

            with Bad _ as e -> state.Add(aI.Id, Choice2Of2 e)

        let parseAppSettings () = List.fold parseAppSettingsPartial Map.empty argInfo


        // does what the type signature says; combines two parse states into one according to the provided rules.
        let combine (appSettingsResults : Map<ArgId, Choice<ParseResult<'Template> list, exn>>)
                    (commandLineResults : Map<ArgId, ParseResult<'Template> list>) =

            let combineSingle (argInfo : ArgInfo) =
                let acr = defaultArg (appSettingsResults.TryFind argInfo.Id) <| Choice1Of2 []
                let clr = defaultArg (commandLineResults.TryFind argInfo.Id) []

                let combined =
                    match acr, clr with
                    | Choice1Of2 ts, [] -> ts
                    | Choice2Of2 e, [] -> raise e
                    | Choice2Of2 e, _ when argInfo.GatherAllSources -> raise e
                    | Choice1Of2 ts, ts' when argInfo.GatherAllSources -> ts @ ts'
                    | _, ts' -> ts'

                match combined with
                | [] when argInfo.Mandatory -> bad ErrorCode.PostProcess "missing argument '%s'." <| getName argInfo
                | _ -> combined

            argInfo |> Seq.map (fun aI -> aI.Id, (aI, combineSingle aI)) |> Map.ofSeq

        member s.ParseCommandLine (?inputs : string [], ?errorHandler: Exiter) =
            let errorHandler = defaultArg errorHandler exceptionExiter
            let inputs = match inputs with None -> getEnvArgs () | Some args -> args

            try
                let results = combine Map.empty (parseCommandLine inputs)

                ArgParseResults<_>(s, results, errorHandler)
            with
            | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

        member s.ParseAppSettings (?errorHandler: Exiter) =
            let errorHandler = defaultArg errorHandler exceptionExiter
            
            try
                let results = combine (parseAppSettings()) Map.empty

                ArgParseResults<_>(s, results, errorHandler)
            with
            | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

        member s.Parse (?inputs : string [], ?errorHandler : Exiter) =
            let errorHandler = defaultArg errorHandler exceptionExiter
            let inputs = match inputs with None -> getEnvArgs () | Some args -> args

            try
                let results = combine (parseAppSettings()) (parseCommandLine inputs)

                ArgParseResults<_>(s, results, errorHandler)
            with
            | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

        member __.Usage ?msg = usage msg argInfo

        member __.Print (args : 'Template list) =
            let printEntry (t : 'Template) =
                let uci, fields = FSharpValue.GetUnionFields(t, typeof<'Template>)
                let id = ArgId uci
                let aI = argInfo |> List.find (fun aI -> id = aI.Id)

                seq {
                    match aI.CommandLineNames with
                    | [] -> ()
                    | clname :: _ ->
                        yield clname

                        for f in fields do
                            if f = null then yield null
                            else yield f.ToString()
                }

            seq {
                for t in args do yield! printEntry t
            } |> Seq.toArray

        member a.PrintFlat (args : 'Template list) =
            a.Print args |> Seq.map (fun str -> "\"" + str + "\"") |> String.concat " "
            
    // argument parsing result holder 
    and ArgParseResults<'Template when 'Template :> IArgParserTemplate> 
            internal (ap : UnionArgParser<'Template>, results : Map<ArgId, ArgInfo * ParseResult<'Template> list>, exiter : Exiter) =

        let exit msg id = exiter.Exit(ap.Usage msg, id)
        let getResults (e : Expr) = results.[expr2ArgId e] |> snd
        let containsResult (e : Expr) = e |> getResults |> Seq.isEmpty |> not
        let tryGetResult (e : Expr) = e |> getResults |> List.tryLast
        let getResult (e : Expr) =
            let id = expr2ArgId e
            let aI, results = results.[id]
            match List.tryLast results with
            | None -> exit (sprintf "missing argument '%s'." <| getName aI) (int ErrorCode.PostProcess)
            | Some r -> r

        let parseResult (f : 'F -> 'S) (r : ParseResult<'T>) =
            try f (r.FieldContents :?> 'F)
            with e ->
                exit (sprintf "Error parsing '%s': %s." r.ParseContext e.Message) (int ErrorCode.PostProcess)

        member __.GetResults (expr : Expr<'Template>) = expr |> getResults |> List.map (fun r -> r.Value)
        member __.GetResults (expr : Expr<'Fields -> 'Template>) = expr |> getResults |> List.map (fun r -> r.FieldContents :?> 'Fields)
        member __.GetResults () = 
            results 
            |> Seq.collect (fun (KeyValue(_,(_,rs))) -> rs) 
            |> Seq.map (fun r -> r.Value)
            |> Seq.toList

        member __.TryGetResult (expr : Expr<'Template>) = expr |> tryGetResult |> Option.map (fun r -> r.Value)
        member __.TryGetResult (expr : Expr<'Fields -> 'Template>) = 
            expr |> tryGetResult |> Option.map (fun r -> r.FieldContents :?> 'Fields)

        member __.GetResult (expr : Expr<'Template>) = let r = getResult expr in r.FieldContents :?> 'Fields
        member s.GetResult (expr : Expr<'Fields -> 'Template>, ?defaultValue : 'Fields) =
            match defaultValue with
            | None -> let r = getResult expr in r.FieldContents :?> 'Fields
            | Some def -> defaultArg (s.TryGetResult expr) def

        member __.Contains (expr : Expr<'Template>) = containsResult expr
        member __.Contains (expr : Expr<_ -> 'Template>) = containsResult expr

        member __.Raise (msg, ?errorCode) : 'T = exit msg (defaultArg errorCode (int ErrorCode.PostProcess))
        member r.Raise (e : exn, ?errorCode) : 'T = 
            r.Raise (e.Message, ?errorCode = errorCode)
        member r.Catch (f : unit -> 'T, ?errorCode) =
            try f () with e -> r.Raise(e.Message, ?errorCode = errorCode)

        member r.PostProcessResult (expr : Expr<'Field -> 'Template>) (parser : 'Field -> 'R) =
            expr |> getResult |> parseResult parser

        member r.PostProcessResults (expr : Expr<'Field -> 'Template>) (parser : 'Field -> 'R) =
            expr |> getResults |> List.map (parseResult parser)

        member r.TryPostProcessResult (expr : Expr<'Field -> 'Template>) (parser : 'Field -> 'R) =
            expr |> tryGetResult |> Option.map (parseResult parser)
