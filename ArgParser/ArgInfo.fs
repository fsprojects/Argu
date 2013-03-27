module internal UnionArgParser.InternalImpls

    open System
    open System.Configuration
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    open UnionArgParser.Utils

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
    let expr2ArgId (e : Expr) =
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
                                    failwithf "UnionArgParser: parameter '%s' is reserved for the 'usage' parameter." n
                                if n.ToCharArray() |> Array.forall (fun c -> Char.IsLetterOrDigit c || c = '-') |> not then
                                    failwithf "UnionArgParser: parameter '%s' contains invalid characters." n)

                clNames

        let AppSettingsName =
            if attrReader.ContainsAttr<NoAppSettingsAttribute> (true) then None
            else
                match attrReader.GetAttrs<CustomAppSettingsAttribute> () |> List.tryLast with
                | None -> Some <| uciToAppConf uci
                // take last registered attribute
                | Some attr -> Some attr.Name

        if AppSettingsName.IsNone && commandLineArgs.IsEmpty then 
            failwith "UnionArgParser: parameter '%s' needs to have at least one parse source." uci.Name

        let parsers =
            let getPrimParser (t : Type) =
                match primIdx.TryFind t with
                | Some pp -> pp
                | None -> failwithf "UnionArgParser: template contains unsupported field of type %A." t

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
            failwith "UnionArgParser: CSV attribute is only compatible with branches of unary fields." 

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
            new Exiter with member __.Exit (msg,_) = raise <| ArgumentException(defaultArg msg "UnionArgParser error")
        } 