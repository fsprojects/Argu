[<AutoOpen>]
module internal Argu.Generator

open System
open System.Reflection
open System.Text.RegularExpressions

open FSharp.Reflection

exception HelpText
exception Bad of string * ErrorCode * UnionCaseArgInfo option

let inline bad code aI fmt = Printf.ksprintf (fun msg -> raise <| Bad(msg, code, aI)) fmt

///// checks if given parameter name is contained in argument
//let hasCommandLineParam (aI : UnionCaseArgInfo) (param : string) =
//    aI.CommandLineNames |> List.exists ((=) param)

/// construct a CLI param from UCI name
let uciToOpt (uci : UnionCaseInfo) =
    let prefix = 
        uci.TryGetAttribute<CliPrefixAttribute>(true) 
        |> Option.map (fun a -> a.Prefix)
        |> id (fun p -> defaultArg p CliPrefix.DoubleDash)

    let prefixString =
        match prefix with
        | CliPrefix.DoubleDash -> "--" 
        | CliPrefix.Dash -> "-" 
        | CliPrefix.Empty -> "" 
        | p -> invalidArg "CliPrefix" <| sprintf "unsupported CLI prefix '%s'." (string p)

    prefixString + uci.Name.ToLower().Replace('_','-')

/// construct an App.Config param from UCI name
let uciToAppConf (uci : UnionCaseInfo) =
    uci.Name.ToLower().Replace('_',' ')

/// get CL arguments from environment
let getEnvArgs () =
    match System.Environment.GetCommandLineArgs() with
    | [||] -> [||]
    | args -> args.[1..]

/// Creates a primitive field parser from given parser/unparser lambdas
let mkPrimParser (name : string) (parser : string -> 'T) (unparser : 'T -> string) (label : string option) =
    {
        Name = name
        Label = label
        Type = typeof<'T>
        Parser = fun x -> parser x :> obj
        UnParser = fun o -> unparser (o :?> 'T)
    }
        
/// dummy argInfo for --help arg
let helpInfo : UnionCaseArgInfo = 
    {
        Name = "--help"
        UnionCaseInfo = Unchecked.defaultof<_>
        CommandLineNames = ["--help" ; "-h" ; "/h" ; "/help" ; "/?"]
        Usage = "display this list of options."
        AppSettingsName = None
        FieldParsers = Primitives [||]
        CaseCtor = fun _ -> invalidOp "internal error: attempting to use '--help' case constructor."
        FieldCtor = None
        FieldReader = fun _ -> invalidOp "internal error: attempting to use '--help' case constructor."
        PrintLabels = false ;
        Hidden = false ; AppSettingsCSV = false ; Mandatory = false ; 
        GatherAllSources = false ; IsRest = false ; IsFirst = false
        IsEqualsAssignment = false ; IsHelpParameter = true
    }

let primitiveParsers =
    let mkParser name pars unpars = typeof<'T>, mkPrimParser name pars unpars in
    dict [|
        mkParser "bool" Boolean.Parse (fun b -> if b then "true" else "false")
        mkParser "byte" Byte.Parse string
        mkParser "sbyte" SByte.Parse string

        mkParser "int16" Int16.Parse string
        mkParser "int" Int32.Parse string
        mkParser "int64" Int64.Parse string
        mkParser "uint16" UInt16.Parse string
        mkParser "uint" UInt32.Parse string
        mkParser "uint64" UInt64.Parse string

        mkParser "char" Char.Parse string
        mkParser "string" id id

        mkParser "float" Single.Parse string
        mkParser "float" Double.Parse string
        mkParser "decimal" Decimal.Parse string
#if !NET35
        mkParser "bigint" System.Numerics.BigInteger.Parse string
#endif
        mkParser "guid" (fun s -> Guid(s)) string

        mkParser "base64" Convert.FromBase64String Convert.ToBase64String
    |]


let (|UnionParseResult|_|) (t : Type) =
    if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<ParseResults<_>> then
        Some(t.GetGenericArguments().[0])
    else None

let private validCliParamRegex = new Regex("^[\w]+$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let private validAppSettingsParamRegex = new Regex("^[0-9a-z \-\=]+$", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

/// generate argument parsing schema from given UnionCaseInfo
let rec preComputeUnionCaseArgInfo (stack : Type list) (uci : UnionCaseInfo) : UnionCaseArgInfo =
    let fields = uci.GetFields()
    let types = fields |> Array.map (fun f -> f.PropertyType)

    let caseCtor = FSharpValue.PreComputeUnionConstructor(uci, bindingFlags = allBindings)

    /// create a dummy instance for the current union case
    let usageString = 
        let dummyFields = types |> Array.map Unchecked.UntypedDefaultOf
        let dummy = caseCtor dummyFields :?> IArgParserTemplate
        dummy.Usage
        
    let commandLineArgs =
        if uci.ContainsAttribute<NoCommandLineAttribute> (true) then []
        else
            let defaultName =
                match uci.TryGetAttribute<CustomCommandLineAttribute> () with 
                | None -> uciToOpt uci
                | Some attr -> attr.Name

            let altNames =
                uci.GetAttributes<AltCommandLineAttribute> ()
                |> Seq.collect (fun attr -> attr.Names)
                |> Seq.toList

            let cliNames = defaultName :: altNames

            for name in cliNames do
                if not <| validCliParamRegex.IsMatch name then
                    failwithf "Argu: CLI parameter '%s' contains invalid characters." name

            cliNames

//    let singleCharacterSwitches =
//        if fields.Length = 0 then
//            commandLineArgs 
//            |> Seq.filter (fun arg -> 
//                arg.Length = 2 && 
//                arg.[0] = '-' && 
//                Char.IsLetterOrDigit arg.[1])
//            |> Seq.map (fun arg -> arg.[1])
//            |> Seq.toArray
//        else [||]

    let appSettingsName =
        if uci.ContainsAttribute<NoAppSettingsAttribute> (true) then None
        else
            match uci.TryGetAttribute<CustomAppSettingsAttribute> () with
            | None -> Some <| uciToAppConf uci
            // take last registered attribute
            | Some attr when validAppSettingsParamRegex.IsMatch attr.Name -> Some attr.Name
            | Some attr -> failwithf "Argu: AppSettings parameter '%s' contains invalid parameters." attr.Name

    /// gets the default name of the argument
    let defaultName =
        match commandLineArgs with
        | h :: _ -> h
        | _ when Option.isSome appSettingsName -> appSettingsName.Value
        | _ -> failwithf "Argu: parameter '%s' needs to have at least one parse source." uci.Name

    let printLabels = uci.ContainsAttribute<PrintLabelsAttribute> (true)

    let parsers =
        let getParser (p : PropertyInfo) =
            let label = if printLabels then Some p.Name else None
            let ok, f = primitiveParsers.TryGetValue p.PropertyType
            if ok then f label
            else
                failwithf "Argu: template contains unsupported field of type '%O'." p.PropertyType

        match types with
        | [|UnionParseResult prt|] -> preComputeUnionArgInfo stack prt |> NestedUnion
        | _ ->  Array.map getParser fields |> Primitives

    let fieldCtor =
        match types.Length with
        | 0 -> None
        | 1 -> Some(fun (o:obj[]) -> o.[0])
        | _ ->
            let tupleType = FSharpType.MakeTupleType types
            let ctor = FSharpValue.PreComputeTupleConstructor tupleType
            Some ctor

    let fieldReader = FSharpValue.PreComputeUnionReader(uci, bindingFlags = allBindings)

    let appSettingsCSV = uci.ContainsAttribute<ParseCSVAttribute> ()
    let mandatory = uci.ContainsAttribute<MandatoryAttribute> (true)
    let gatherAll = uci.ContainsAttribute<GatherAllSourcesAttribute> ()
    let isRest = uci.ContainsAttribute<RestAttribute> ()
    let isHidden = uci.ContainsAttribute<HiddenAttribute> ()
    let isHelpParameter = uci.ContainsAttribute<HelpAttribute> ()
    let isEqualsAssignment = 
        if uci.ContainsAttribute<EqualsAssignmentAttribute> (true) then
            if types.Length <> 1 then
                failwithf "Argu: Parameter '%s' has EqualsAssignment attribute but has arity <> 1." uci.Name
            elif isRest then
                failwithf "Argu: Parameter '%s' contains incompatible attributes 'EqualsAssignment' and 'Rest'." uci.Name
            true
        else
            false

    let first = uci.ContainsAttribute<FirstAttribute> ()

    if appSettingsCSV && fields.Length <> 1 then 
        failwith "Argu: CSV attribute is only compatible with branches of unary fields." 

    {
        UnionCaseInfo = uci
        CaseCtor = caseCtor
        FieldReader = fieldReader
        FieldCtor = fieldCtor
        Name = defaultName
        CommandLineNames = commandLineArgs
        AppSettingsName = appSettingsName
        Usage = usageString
        FieldParsers = parsers
        AppSettingsCSV = appSettingsCSV
        Mandatory = mandatory
        PrintLabels = printLabels
        GatherAllSources = gatherAll
        IsRest = isRest
        IsFirst = first
        IsEqualsAssignment = isEqualsAssignment
        Hidden = isHidden
        IsHelpParameter = isHelpParameter
    }

and preComputeUnionArgInfo (stack : Type list) (t : Type) : UnionArgInfo =
    if not <| FSharpType.IsUnion(typeof<'Template>, bindingFlags = allBindings) then
        invalidArg typeof<'Template>.Name "Argu: template type is not F# DU."
    elif stack |> List.exists ((=) t) then
        invalidArg typeof<'Template>.Name "Argu: template type implements unsupported recursive pattern."
    elif t.IsGenericType then
        invalidArg typeof<'Template>.Name "Argu: template type is generic which is not supported."

    let caseInfo =
        FSharpType.GetUnionCases(typeof<'Template>, bindingFlags = allBindings)
        |> Seq.map (preComputeUnionCaseArgInfo (t :: stack))
        |> Seq.sortBy (fun a -> a.Tag)
        |> Seq.toArray

    let useDefaultHelperSwitches =
        if caseInfo |> Array.exists (fun c -> c.IsHelpParameter) then false
        else t.ContainsAttribute<DisableHelpAttribute>() |> not

    // check for conflicting CLI identifiers
    let cliConflicts =
        caseInfo
        |> Seq.collect(fun arg -> arg.CommandLineNames |> Seq.map (fun name -> arg, name))
        |> Seq.groupBy snd
        |> Seq.choose(fun (name, args) -> if Seq.length args > 1 then Some(name, args |> Seq.map fst |> Seq.toList) else None)
        |> Seq.toList

    match cliConflicts with
    | (id, arg0 :: arg1 :: _) :: _ -> 
        let msg = sprintf "Argu: Parameters '%O' and '%O' using conflicting CLI identifier '%s'." arg0.UnionCaseInfo arg1.UnionCaseInfo id
        raise <| new FormatException(msg)
    | _ -> ()

    // check for conflicting CLI identifiers
    let appSettingsConflicts =
        caseInfo
        |> Seq.choose(fun arg -> arg.AppSettingsName |> Option.map (fun name -> arg, name))
        |> Seq.groupBy snd
        |> Seq.choose(fun (name, args) -> if Seq.length args > 1 then Some(name, args |> Seq.map fst |> Seq.toList) else None)
        |> Seq.toList

    match appSettingsConflicts with
    | (id, arg0 :: arg1 :: _) :: _ ->
        let msg = sprintf "Argu: Parameters '%O' and '%O' using conflicting AppSettings identifier '%s'." arg0.UCI arg1.UCI id
        raise <| new FormatException(msg)
    | _ -> ()

    {
        Type = t
        Parents = 
    
    }



let isAppConfig (aI : UnionCaseArgInfo) = aI.AppSettingsName.IsSome
let isCommandLine (aI : UnionCaseArgInfo) = not aI.CommandLineNames.IsEmpty


/// construct a parse result from untyped collection of parsed arguments
let buildResult<'T> (argInfo : UnionCaseArgInfo) src ctx (fields : obj []) =
    {
        Value = argInfo.CaseCtor fields :?> 'T
        FieldContents =
            match argInfo.FieldCtor with
            | None -> null
            | Some ctor -> ctor fields

        UnionCaseArgInfo = argInfo
        Source = src
        ParseContext = ctx
    }