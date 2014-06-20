module internal Nessos.UnionArgParser.ArgInfo

    open System
    open System.Configuration
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    open Nessos.UnionArgParser.Utils

    type ErrorCode =
        | HelpText = 0
        | AppSettings = 2
        | CommandLine = 3
        | PostProcess = 4

    /// IComparable UnionCaseInfo wrapper
    type ArgId(uci : UnionCaseInfo) =
        inherit ProjectionComparison<ArgId,int>(uci.Tag)
        member __.UCI = uci
                    
    and ArgInfo =
        {
            /// Argument identifier
            Id : ArgId

            /// Field parser definitions
            FieldParsers : FieldInfo []

            /// Builds a union case out of its field parameters
            CaseCtor : obj [] -> obj
            /// Composes case fields into a tuple, if not nullary
            FieldCtor : (obj [] -> obj) option

            /// head element denotes primary command line arg
            CommandLineNames : string list
            /// name used in AppSettings
            AppSettingsName : string option

            /// Description of the parameter
            Usage : string

            /// If specified, should consume remaining tokens from the CLI
            IsRest : bool
            /// If specified, parameter can only be at start of CLI parameters
            IsFirst : bool
            /// Print labels in Usage ()
            PrintLabels : bool
            /// If specified, multiple parameters can be added in AppSettings in CSV form.
            AppSettingsCSV : bool
            /// Fails if no argument of this type is specified
            Mandatory : bool
            /// Hide from Usage
            Hidden : bool
            /// Combine AppSettings with CLI inputs
            GatherAllSources : bool
        }
    with
        member __.UCI = __.Id.UCI
        member __.NoCommandLine = __.CommandLineNames.IsEmpty

    and ParseResult<'T> =
        {
            Value : 'T // union case value
            FieldContents : obj // untyped version of tuple of branch contents
            ArgInfo : ArgInfo
            ParseContext : string // metadata given by the parser
            Source : ParseSource
        }

    and FieldInfo =
        {
            /// Type name
            Name : string
            /// field label
            Label : string option
            /// field type
            Type : Type
            /// parser
            Parser : string -> obj
        }
    with
        override p.ToString() =
            match p.Label with
            | None -> p.Name
            | Some l -> sprintf "%s:%s" l p.Name

    exception HelpText
    exception Bad of string * ErrorCode * ArgInfo option

    let bad code aI fmt = Printf.ksprintf (fun msg -> raise <| Bad(msg, code, aI)) fmt

    // gets the default name of the argument
    let getName (aI : ArgInfo) =
        match aI.CommandLineNames, aI.AppSettingsName with
        | name :: _, _ -> name
        | [], Some name -> name
        | [], None -> failwith "impossible"

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
            FieldParsers = [||]
            CaseCtor = fun _ -> invalidOp "internal error: attempting to '--help' case constructor."
            FieldCtor = None
            PrintLabels = false ;
            Hidden = false ; AppSettingsCSV = false ; Mandatory = false ; 
            GatherAllSources = false ; IsRest = false ; IsFirst = false
        }


    let mkParserBuilder (name : string) (parser : string -> 'T) =
        let t = typeof<'T>
        let mkParser label =
            {
                Name = name
                Label = label
                Type = typeof<'T>
                Parser = fun x -> parser x :> obj
            }
        t, mkParser

    let parserBuilderIdx =
        dict [
            mkParserBuilder "bool" Boolean.Parse
            mkParserBuilder "char" Char.Parse
            mkParserBuilder "uint16" UInt16.Parse
            mkParserBuilder "uint" UInt32.Parse
            mkParserBuilder "uint64" UInt64.Parse
            mkParserBuilder "int16" Int16.Parse
            mkParserBuilder "int" Int32.Parse
            mkParserBuilder "int64" Int64.Parse
            mkParserBuilder "byte" Byte.Parse
            mkParserBuilder "sbyte" SByte.Parse
            mkParserBuilder "string" id
            mkParserBuilder "float" Single.Parse
            mkParserBuilder "float" Double.Parse
            mkParserBuilder "decimal" Decimal.Parse
#if NET35
#else
            mkParserBuilder "bigint" System.Numerics.BigInteger.Parse
#endif
            mkParserBuilder "guid" (fun s -> Guid(s))
        ]

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
            
        let caseCtor = FSharpValue.PreComputeUnionConstructor(uci, bindingFlags = allBindings)

        let dummy = 
            let dummyFields = types |> Array.map Unchecked.UntypedDefaultOf
            caseCtor dummyFields :?> IArgParserTemplate
        
        let commandLineArgs =
            if uci.ContainsAttr<NoCommandLineAttribute> (true) then []
            else
                let defName = 
                    match uci.GetAttrs<CustomCommandLineAttribute> () |> List.tryLast with 
                    | None -> uciToOpt uci
                    | Some attr -> attr.Name

                let altNames = 
                    uci.GetAttrs<AltCommandLineAttribute> ()
                    |> List.map (fun attr -> attr.Name)

                let clNames = defName :: altNames 

                for name in clNames do
                    if hasCommandLineParam helpInfo name then
                        failwithf "UnionArgParser: parameter '%s' is reserved for the 'usage' parameter." name
                    if name.ToCharArray() |> Array.forall (fun c -> Char.IsLetterOrDigit c || c = '-') |> not then
                        failwithf "UnionArgParser: parameter '%s' contains invalid characters." name

                clNames

        let AppSettingsName =
            if uci.ContainsAttr<NoAppSettingsAttribute> (true) then None
            else
                match uci.GetAttrs<CustomAppSettingsAttribute> () |> List.tryLast with
                | None -> Some <| uciToAppConf uci
                // take last registered attribute
                | Some attr -> Some attr.Name

        if AppSettingsName.IsNone && commandLineArgs.IsEmpty then 
            failwith "UnionArgParser: parameter '%s' needs to have at least one parse source." uci.Name

        let printLabels = uci.ContainsAttr<PrintLabelsAttribute> (true)

        let parsers =
            let getPrimParser (p : PropertyInfo) =
                let label = if printLabels then Some p.Name else None
                match parserBuilderIdx.TryFind p.PropertyType with
                | Some f -> f label
                | None -> 
                    failwithf "UnionArgParser: template contains unsupported field of type '%O'." p.PropertyType

            Array.map getPrimParser fields

        let fieldCtor =
            match types.Length with
            | 0 -> None
            | 1 -> Some(fun (o:obj[]) -> o.[0])
            | _ ->
                let tupleType = FSharpType.MakeTupleType types
                let ctor = FSharpValue.PreComputeTupleConstructor tupleType
                Some ctor

        let AppSettingsCSV = uci.ContainsAttr<ParseCSVAttribute> ()
        let mandatory = uci.ContainsAttr<MandatoryAttribute> (true)
        let gatherAll = uci.ContainsAttr<GatherAllSourcesAttribute> ()
        let isRest = uci.ContainsAttr<RestAttribute> ()
        let isHidden = uci.ContainsAttr<HiddenAttribute> ()
        let first = uci.ContainsAttr<FirstAttribute> ()

        if AppSettingsCSV && fields.Length <> 1 then 
            failwith "UnionArgParser: CSV attribute is only compatible with branches of unary fields." 

        {
            Id = ArgId uci
            CaseCtor = caseCtor
            FieldCtor = fieldCtor
            CommandLineNames = commandLineArgs
            AppSettingsName = AppSettingsName
            Usage = dummy.Usage
            FieldParsers = parsers
            AppSettingsCSV = AppSettingsCSV
            Mandatory = mandatory
            PrintLabels = printLabels
            GatherAllSources = gatherAll
            IsRest = isRest
            IsFirst = first
            Hidden = isHidden
        }


    let buildResult<'T> (argInfo : ArgInfo) src ctx (fields : obj []) =
        {
            Value = argInfo.CaseCtor fields :?> 'T
            FieldContents =
                match argInfo.FieldCtor with
                | None -> null
                | Some ctor -> ctor fields

            ArgInfo = argInfo
            Source = src
            ParseContext = ctx
        }

    let isAppConfig (aI : ArgInfo) = aI.AppSettingsName.IsSome
    let isCommandLine (aI : ArgInfo) = not aI.CommandLineNames.IsEmpty