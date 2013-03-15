namespace ArgParser

    open System
    open System.Configuration
    open System.Reflection

    open Microsoft.FSharp.Reflection

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    open ArgParser.Utils

    /// Parse comma separated values in AppSettings
    type ParseCSVAttribute () = inherit Attribute ()
    /// Consume all remaining command line arguments.
    type RestAttribute () = inherit Attribute ()
    /// Hide from command line argument documentation.
    type HiddenAttribute () = inherit Attribute ()
    /// Demands at least one parsed result for this branch; an exception is raised otherwise.
    type MandatoryAttribute () = inherit Attribute ()
    /// Gathers all parsed results from both AppSettings and command line.
    type GatherAllSourcesAttribute () = inherit Attribute ()
    /// Disable command line parsing for this branch.
    type NoCommandLineAttribute () = inherit Attribute ()
    /// Disable AppSettings parsing for this branch.
    type NoAppSettingsAttribute () = inherit Attribute ()

    /// Sets a custom command line name.
    type CustomCommandLineAttribute (name : string) =
        inherit Attribute ()
        member __.Name = name

    /// Sets alternative command line names.
    type AltCommandLineAttribute (name : string) = 
        inherit Attribute ()
        member __.Name = name

    /// Sets a custom AppSettings key name.
    type CustomAppSettingsAttribute (name : string) = 
        inherit Attribute ()
        member __.Name = name


    type IArgParserTemplate =
        abstract Usage : string

    and Exiter =
        abstract Exit : ?msg : string * ?id : int -> 'T



    module internal Implementations =

        type ErrorCode =
            | HelpText = 0
            | AppSettings = 2
            | CommandLine = 3
            | PostProcess = 4
        
        exception HelpText
        exception Bad of ErrorCode * string

        let bad code fmt = Printf.ksprintf (fun msg -> Bad(code, msg) |> raise) fmt

        type ArgId(uci : UnionCaseInfo) =
            inherit ProjectionComparison<ArgId,int>(uci.Tag)
            member __.UCI = uci
                    
        and ArgInfo =
            {
                Id : ArgId
                Parsers : PrimitiveParser []
                ReflectedFields : Type option

                CommandLineNames : string list // head element denotes primary command line arg
                AppSettingsName : string option
                Usage : string

                Rest : bool
                AppSettingsCSV : bool
                Mandatory : bool
                Hidden : bool
                GatherAllSources : bool
            }
        with
            member __.UCI = __.Id.UCI

        and ParseResult<'T> =
            {
                Value : 'T // union case value
                FieldContents : obj // untyped version of tuple of branch contents
                ArgInfo : ArgInfo
                ParseContext : string // metadata given by the parser
            }

        and PrimitiveParser =
            {
                Name : string
                Type : Type
                Parser : string -> obj
            }

        let getName (aI : ArgInfo) =
            match aI.CommandLineNames, aI.AppSettingsName with
            | name :: _, _ -> name
            | [], Some name -> name
            | [], None -> failwith "unexpected configuration"

        let hasCommandLineParam (aI : ArgInfo) (param : string) =
            aI.CommandLineNames |> List.exists ((=) param)

        let uciToOpt (uci : UnionCaseInfo) =
            "--" + uci.Name.ToLower().Replace('_','-')

        let uciToAppConf (uci : UnionCaseInfo) =
            uci.Name.ToLower().Replace('_',' ')

        let getEnvArgs () =
            match System.Environment.GetCommandLineArgs() with
            | [||] -> [||]
            | args -> args.[1..]
        
        // dummy argInfo for --help arg
        let helpInfo : ArgInfo = 
            {
                Id = Unchecked.defaultof<_>
                CommandLineNames = ["--help" ; "-h" ; "/h" ; "/help" ; "/?"]
                Usage = "display this list of options."
                AppSettingsName = None
                Parsers = [||]
                ReflectedFields = None
                Hidden = false ; AppSettingsCSV = false ; Mandatory = false ; 
                GatherAllSources = false ; Rest = false
            }


        let mkPrimParser (name : string) (parser : string -> 'T) =
            { Name = name ; Type = typeof<'T> ; Parser = fun x -> parser x :> obj }

        let primParsers =
            [
                mkPrimParser "string" id
                mkPrimParser "int" Int32.Parse
                mkPrimParser "bool" Boolean.Parse
                mkPrimParser "float" Double.Parse
                mkPrimParser "guid" Guid.Parse
            ]

        let primIdx = primParsers |> Seq.map (fun pp -> pp.Type, pp) |> dict

        // recognize exprs that strictly contain DU constructors
        let getId (e : Expr) =
            let rec aux (tupledArg : Var option) vars (e : Expr) =
                match tupledArg, e with
                | None, Lambda(arg, b) -> aux (Some arg) vars b
                | Some arg, Let(x, TupleGet(Var varg, _), b) when arg = varg -> aux tupledArg (x :: vars) b
                | None, NewUnionCase(u, []) -> u
                | Some a, NewUnionCase(u, [Var x]) when a = x -> u
                | Some _, NewUnionCase(u, List.TryMap (|Var|_|) args) when vars.Length > 0 && List.rev vars = args -> u
                | _ -> invalidArg "expr" "Only union constructors are permitted in expression based queries."

            ArgId(aux None [] e)


        let preComputeArgInfo (uci : UnionCaseInfo) : ArgInfo =
            let fields = uci.GetFields()
            let types = fields |> Array.map (fun f -> f.PropertyType)
            let attrReader = UnionCaseAttributeReader uci
            let dummy = FSharpValue.MakeUnion(uci, types |> Array.map defaultOf) :?> IArgParserTemplate
        
            let commandLineArgs =
                if attrReader.ContainsAttr<NoCommandLineAttribute> (true) then []
                else
                    let defName = 
                        match attrReader.GetAttrs<CustomCommandLineAttribute> () |> List.tryLast with 
                        | None -> uciToOpt uci
                        | Some attr -> attr.Name

                    let altNames = 
                        attrReader.GetAttrs<AltCommandLineAttribute> ()
                        |> List.map (fun attr -> attr.Name)

                    let clNames = defName :: altNames 

                    clNames
                    |> List.iter (fun n -> 
                                    if hasCommandLineParam helpInfo n then
                                        failwithf "ArgParser: parameter '%s' is reserved for the 'usage' parameter." n
                                    if n.ToCharArray() |> Array.forall (fun c -> Char.IsLetterOrDigit c || c = '-') |> not then
                                        failwithf "ArgParser: parameter '%s' contains invalid characters." n)

                    clNames

            let AppSettingsName =
                if attrReader.ContainsAttr<NoAppSettingsAttribute> (true) then None
                else
                    match attrReader.GetAttrs<CustomAppSettingsAttribute> () |> List.tryLast with
                    | None -> Some <| uciToAppConf uci
                    // take last registered attribute
                    | Some attr -> Some attr.Name

            if AppSettingsName.IsNone && commandLineArgs.IsEmpty then 
                failwith "ArgParser: parameter '%s' needs to have at least one parse source." uci.Name

            let parsers =
                let getPrimParser (t : Type) =
                    match primIdx.TryFind t with
                    | Some pp -> pp
                    | None -> failwithf "ArgParser: template contains unsupported field of type %A." t

                Array.map getPrimParser types

            let tuple =
                if fields.Length <= 1 then None
                else Some <| FSharpType.MakeTupleType types

            let AppSettingsCSV = attrReader.ContainsAttr<ParseCSVAttribute> ()
            let mandatory = attrReader.ContainsAttr<MandatoryAttribute> (true)
            let gatherAll = attrReader.ContainsAttr<GatherAllSourcesAttribute> ()
            let isRest = attrReader.ContainsAttr<RestAttribute> ()
            let isHidden = attrReader.ContainsAttr<HiddenAttribute> ()

            if AppSettingsCSV && fields.Length <> 1 then 
                failwith "ArgParser: CSV attribute is only compatible with branches of unary fields." 

            {
                Id = ArgId uci
                ReflectedFields = tuple
                CommandLineNames = commandLineArgs
                AppSettingsName = AppSettingsName
                Usage = dummy.Usage
                Parsers = parsers
                AppSettingsCSV = AppSettingsCSV
                Mandatory = mandatory
                GatherAllSources = gatherAll
                Rest = isRest
                Hidden = isHidden
            }

        let printArgInfo (aI : ArgInfo) =
            string {
                match aI.CommandLineNames with
                | [] -> ()
                | param :: altParams ->
                    yield '\t'
                    yield param

                    match altParams with
                    | [] -> ()
                    | h :: rest ->
                        yield " ["
                        yield h
                        for n in rest do
                            yield '|'
                            yield n
                        yield ']'

                    for p in aI.Parsers -> sprintf " <%s>" p.Name

                    if aI.Rest then yield sprintf " ..."

                    yield ": "
                    yield aI.Usage
                    yield "\n"
            }

        let usage (msg : string option) (argInfo : ArgInfo list) =
            string {
                match msg with
                | None -> ()
                | Some u -> yield u + "\n"

                for aI in argInfo do 
                    if not aI.Hidden then
                        yield! printArgInfo aI

                yield! printArgInfo helpInfo
            } |> String.build

        let buildResult<'T> (argInfo : ArgInfo) ctx (fields : obj []) =
            {
                Value = FSharpValue.MakeUnion(argInfo.UCI, fields) :?> 'T
                FieldContents =
                    match fields.Length, argInfo.ReflectedFields with
                    | 0, _ -> () :> obj
                    | 1, _ -> fields.[0]
                    | _, Some t -> FSharpValue.MakeTuple(fields, t)
                    | _, None -> failwith "impossible"

                ArgInfo = argInfo
                ParseContext = ctx
            }


        let exceptionExiter = 
            { 
                new Exiter with member __.Exit (msg,_) = raise <| ArgumentException(defaultArg msg "ArgParser error")
            } 



    open Implementations

    type ArgParser<'Template when 'Template :> IArgParserTemplate> (?usageText : string) =
        do 
            if not <| FSharpType.IsUnion typeof<'Template> then
                failwith "ArgParser: template type must be F# DU."

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
            
            
    and ArgParseResults<'Template when 'Template :> IArgParserTemplate> 
            internal (ap : ArgParser<'Template>, results : Map<ArgId, ArgInfo * ParseResult<'Template> list>, exiter : Exiter) =

        let exit msg id = exiter.Exit(ap.Usage msg, id)
        let getResults (e : Expr) = results.[getId e] |> snd
        let containsResult (e : Expr) = e |> getResults |> Seq.isEmpty |> not
        let tryGetResult (e : Expr) = e |> getResults |> List.tryLast
        let getResult (e : Expr) =
            let id = getId e
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
