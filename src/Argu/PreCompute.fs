[<AutoOpen>]
module internal Argu.PreCompute

#nowarn "44"

open System
open System.Reflection
open System.Collections.Generic
open System.Text.RegularExpressions

open FSharp.Reflection

let defaultHelpParam = "help"
let defaultHelpDescription = "display this list of options."

let getDefaultHelpParam (t : Type) =
    let prefixString =
        match t.TryGetAttribute<CliPrefixAttribute>() with
        | None -> CliPrefix.DoubleDash
        | Some pf -> pf.Prefix

    prefixString + defaultHelpParam

/// construct a CLI param from UCI name
let generateOptionName (uci : UnionCaseInfo) =
    let prefixString = 
        match uci.TryGetAttribute<CliPrefixAttribute>(true) with
        | None -> CliPrefix.DoubleDash
        | Some pf -> pf.Prefix

    prefixString + uci.Name.ToLower().Replace('_','-')

/// construct an App.Config param from UCI name
let generateAppSettingsName (uci : UnionCaseInfo) =
    uci.Name.ToLower().Replace('_',' ')

/// get CL arguments from environment
let getEnvironmentCommandLineArgs () =
    match System.Environment.GetCommandLineArgs() with
    | [||] -> [||]
    | args -> args.[1..]

/// Creates a primitive field parser from given parser/unparser lambdas
let mkPrimitiveParser (name : string) (parser : string -> 'T) (unparser : 'T -> string) (label : string option) =
    {
        Name = name
        Label = label
        Type = typeof<'T>
        Parser = fun x -> parser x :> obj
        UnParser = fun o -> unparser (o :?> 'T)
    }

let primitiveParsers =
    let mkParser name (pars : string -> 'a) unpars = typeof<'a>, mkPrimitiveParser name pars unpars in
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
        mkParser "guid" Guid string
        mkParser "base64" Convert.FromBase64String Convert.ToBase64String
    |]

let (|UnionParseResult|Optional|List|Other|) (t : Type) =
    if t.IsGenericType then
        let gt = t.GetGenericTypeDefinition()
        if gt = typedefof<ParseResult<_>> then UnionParseResult(t.GetGenericArguments().[0])
        elif gt = typedefof<_ option> then Optional(t.GetGenericArguments().[0])
        elif gt = typedefof<_ list> then List(t.GetGenericArguments().[0])
        else Other
    else Other

let getPrimitiveParserByType label (t : Type) = 
    let ok, f = primitiveParsers.TryGetValue t
    if ok then f label
    else
        // refine error messaging depending on the input time
        match t with
        | UnionParseResult _ -> arguExn "Nested ParserResult<'T> parameters can only occur as standalone parameters in union constructors."
        | Optional _ -> arguExn "F# Option parameters can only occur as standalone parameters in union constructors."
        | List _ -> arguExn "F# List parameters can only occur as standalone parameters in union constructors."
        | _ -> arguExn "template contains unsupported field of type '%O'." t

let private validCliParamRegex = new Regex(@"\S+", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let validateCliParam (name : string) =
    if name = null || not <| validCliParamRegex.IsMatch name then
        arguExn "CLI parameter '%s' contains invalid characters." name
    

/// extracts the subcommand argument hierarchy for given UnionArgInfo
let getHierarchy (uai : UnionArgInfo) =
    let rec aux acc (uai : UnionArgInfo) =
        match uai.TryGetParent () with
        | None -> acc
        | Some ucai -> aux (ucai :: acc) (ucai.GetParent())

    aux [] uai

/// generate argument parsing schema from given UnionCaseInfo
let rec private preComputeUnionCaseArgInfo (stack : Type list) (helpParam : HelpParam option)
                                            (getParent : unit -> UnionArgInfo)
                                            (uci : UnionCaseInfo) : UnionCaseArgInfo =

    let fields = uci.GetFields()
    let types = fields |> Array.map (fun f -> f.PropertyType)

    let caseCtor = FSharpValue.PreComputeUnionConstructor(uci, bindingFlags = allBindings)

    // use ref cell for late binding of parent argInfo
    let current = ref None
    let tryGetCurrent = fun () -> !current

    /// create a dummy instance for the current union case
    let usageString = 
        let dummyFields = types |> Array.map Unchecked.UntypedDefaultOf
        let dummy = caseCtor dummyFields :?> IArgParserTemplate
        try dummy.Usage
        with _ -> 
            arguExn "Error generating usage string from IArgParserTemplate for case %O." uci

    let isFirst = uci.ContainsAttribute<FirstAttribute> ()
    let isAppSettingsCSV = uci.ContainsAttribute<ParseCSVAttribute> ()
    let isMandatory = uci.ContainsAttribute<MandatoryAttribute> (true)
    let isGatherAll = uci.ContainsAttribute<GatherAllSourcesAttribute> ()
    let isRest = uci.ContainsAttribute<RestAttribute> ()
    let isHidden = uci.ContainsAttribute<HiddenAttribute> ()
    let isEquals1Assignment, isEquals2Assignment = 
        if uci.ContainsAttribute<EqualsAssignmentAttribute> (true) then
            if types.Length <> 1 && types.Length <> 2 then
                arguExn "parameter '%s' has EqualsAssignment attribute but specifies %d parameters." uci.Name types.Length
            elif isRest then
                arguExn "parameter '%s' contains incompatible attributes 'EqualsAssignment' and 'Rest'." uci.Name

            types.Length = 1, types.Length = 2
        else
            false, false

    let parsers =
        match types with
        | [|UnionParseResult prt|] -> 
            if isEquals1Assignment then
                arguExn "EqualsAssignment in '%s' not supported for nested union cases." uci.Name
            if isRest then
                arguExn "Rest attribute in '%s' not supported for nested union cases." uci.Name
            if isMandatory then
                arguExn "Mandatory attribute in '%s' not supported for nested union cases." uci.Name

            let argInfo = preComputeUnionArgInfoInner stack helpParam tryGetCurrent prt 
            let shape = ShapeArgumentTemplate.FromType prt
            NestedUnion(shape, argInfo)

        | [|Optional t|] ->
            if isRest then
                arguExn "Rest attribute in '%s' not supported for optional cases." uci.Name

            OptionalParam(Existential.FromType t, getPrimitiveParserByType None t)

        | [|List t|] ->
            if isEquals1Assignment then
                arguExn "EqualsAssignment in '%s' not supported for variadic list cases." uci.Name

            if isRest then
                arguExn "Rest attribute in '%s' not supported for variadic list cases." uci.Name

            ListParam(Existential.FromType t, getPrimitiveParserByType None t)

        | _ -> 
            let getParser index (p : PropertyInfo) =
                let label =
                    if types.Length = 1 && p.Name <> "Item" then Some p.Name
                    elif types.Length > 1 && p.Name <> sprintf "Item%d" index then Some p.Name
                    else None

                getPrimitiveParserByType label p.PropertyType

            Array.mapi getParser fields |> Primitives

    let commandLineArgs =
        if uci.ContainsAttribute<NoCommandLineAttribute> (true) then 
            if parsers.IsNested then
                arguExn "NoCommandLine attribute in '%s' not supported for nested union cases." uci.Name
            []
        else
            let defaultName =
                match uci.TryGetAttribute<CustomCommandLineAttribute> () with 
                | None -> generateOptionName uci
                | Some attr -> attr.Name

            let altNames =
                uci.GetAttributes<AltCommandLineAttribute> ()
                |> Seq.collect (fun attr -> attr.Names)
                |> Seq.toList

            let cliNames = defaultName :: altNames

            for name in cliNames do validateCliParam name

            cliNames

    let appSettingsName =
        if uci.ContainsAttribute<NoAppSettingsAttribute> (true) then None
        else
            match uci.TryGetAttribute<CustomAppSettingsAttribute> () with
            | None -> Some <| generateAppSettingsName uci
            // take last registered attribute
            | Some attr when parsers.IsNested -> arguExn "CustomAppSettings in %s not supported for nested union cases" uci.Name
            | Some attr when not <| String.IsNullOrWhiteSpace attr.Name -> Some attr.Name
            | Some attr -> arguExn "AppSettings parameter '%s' contains invalid characters." attr.Name

    /// gets the default name of the argument
    let defaultName =
        match commandLineArgs with
        | h :: _ -> h
        | _ when Option.isSome appSettingsName -> appSettingsName.Value
        | _ -> arguExn "parameter '%s' needs to have at least one parse source." uci.Name

    let fieldCtor = lazy(
        match types.Length with
        | 0 -> None
        | 1 -> Some(fun (o:obj[]) -> o.[0])
        | _ ->
            let tupleType = FSharpType.MakeTupleType types
            let ctor = FSharpValue.PreComputeTupleConstructor tupleType
            Some ctor)

    let fieldReader = lazy(FSharpValue.PreComputeUnionReader(uci, bindingFlags = allBindings))

    if isAppSettingsCSV && fields.Length <> 1 then 
        arguExn "CSV attribute is only compatible with branches of unary fields." 

    let uai = {
        UnionCaseInfo = uci
        CaseCtor = caseCtor
        FieldReader = fieldReader
        FieldCtor = fieldCtor
        Name = defaultName
        GetParent = getParent
        CommandLineNames = commandLineArgs
        AppSettingsName = appSettingsName
        Usage = usageString
        FieldParsers = parsers
        AppSettingsCSV = isAppSettingsCSV
        IsMandatory = isMandatory
        GatherAllSources = isGatherAll
        IsRest = isRest
        IsFirst = isFirst
        IsEquals1Assignment = isEquals1Assignment
        IsEquals2Assignment = isEquals2Assignment
        IsHidden = isHidden
    }

    current := Some uai // assign result to children
    uai

and private preComputeUnionArgInfoInner (stack : Type list) (helpParam : HelpParam option) (tryGetParent : unit -> UnionCaseArgInfo option) (t : Type) : UnionArgInfo =
    if not <| FSharpType.IsUnion(t, bindingFlags = allBindings) then
        arguExn "template type '%O' is not an F# union." t
    elif stack |> List.exists ((=) t) then
        arguExn "template type '%O' implements unsupported recursive pattern." t
    elif t.IsGenericType then
        arguExn "template type '%O' is generic; this is not supported." t

    let helpParam =
        match helpParam with
        | Some hp -> hp // always inherit help schema from parent union
        | None ->
            let helpSwitches =
                match t.TryGetAttribute<HelpFlagsAttribute>() with
                | None -> [getDefaultHelpParam t]
                | Some hf -> 
                    for f in hf.Names do validateCliParam f
                    Array.toList hf.Names

            let description = 
                match t.TryGetAttribute<HelpDescriptionAttribute> () with
                | None -> defaultHelpDescription
                | Some attr -> attr.Description

            { Flags = helpSwitches ; Description = description }


    // use ref cell for late binding of parent argInfo
    let current = ref Unchecked.defaultof<_>
    let getCurrent = fun () -> !current

    let caseInfo =
        FSharpType.GetUnionCases(t, bindingFlags = allBindings)
        |> Seq.map (preComputeUnionCaseArgInfo (t :: stack) (Some helpParam) getCurrent)
        |> Seq.sortBy (fun a -> a.Tag)
        |> Seq.toArray

    // check for conflicting CLI identifiers
    let cliConflicts =
        caseInfo
        |> Seq.collect (fun arg -> arg.CommandLineNames |> Seq.map (fun name -> arg, name))
        |> Seq.map (fun ((arg, name) as t) ->
            if helpParam.Flags |> List.exists ((=) name) then
                arguExn "parameter '%O' using CLI identifier '%s' which is reserved for help params." arg.UnionCaseInfo name
            t)
        |> Seq.groupBy snd
        |> Seq.map (fun (name, args) -> name, args |> Seq.map fst |> Seq.toArray)
        |> Seq.filter (fun (_,args) -> args.Length > 1)
        |> Seq.sortBy (fun (_,args) -> -args.Length)
        |> Seq.toArray

    if cliConflicts.Length > 0 then
        let id, cs = cliConflicts.[0]
        arguExn "parameters '%O' and '%O' using conflicting CLI identifier '%s'." cs.[0].UnionCaseInfo cs.[1].UnionCaseInfo id

    // check for conflicting CLI identifiers
    let appSettingsConflicts =
        caseInfo
        |> Seq.choose(fun arg -> arg.AppSettingsName |> Option.map (fun name -> arg, name))
        |> Seq.groupBy snd
        |> Seq.map (fun (name, args) -> name, args |> Seq.map fst |> Seq.toArray)
        |> Seq.filter (fun (_,args) -> args.Length > 1)
        |> Seq.sortBy (fun (_,args) -> -args.Length)
        |> Seq.toArray

    if appSettingsConflicts.Length > 0 then
        let id, cs = appSettingsConflicts.[0]
        arguExn "parameters '%O' and '%O' using conflicting AppSettings identifier '%s'." cs.[0].UnionCaseInfo cs.[1].UnionCaseInfo id

    // recognizes and extracts grouped switches
    // e.g. -efx --> -e -f -x
    let groupedSwitchExtractor = lazy(
        let chars =
            caseInfo
            |> Seq.collect (fun c -> c.CommandLineNames) 
            |> Seq.filter (fun name -> name.Length = 2 && name.[0] = '-' && Char.IsLetterOrDigit name.[1])
            |> Seq.map (fun name -> name.[1])
            |> Seq.toArray
            |> String

        if chars.Length = 0 then (fun _ -> [||]) else

        let regex = new Regex(sprintf "^-[%s]+$" chars, RegexOptions.Compiled)

        fun (arg : string) ->
            if not <| regex.IsMatch arg then [||] 
            else Array.init (arg.Length - 1) (fun i -> sprintf "-%c" arg.[i + 1]))

    let tagReader = lazy(FSharpValue.PreComputeUnionTagReader(t, bindingFlags = allBindings))

    let appSettingsIndex = lazy(
        caseInfo
        |> Seq.choose (fun cs -> match cs.AppSettingsName with Some name -> Some(name, cs) | None -> None)
        |> dict)

    let cliIndex = lazy(
        caseInfo
        |> Seq.collect (fun cs -> cs.CommandLineNames |> Seq.map (fun name -> name, cs))
        |> dict)

    let result = {
        Type = t
        TryGetParent = tryGetParent
        Cases = caseInfo
        TagReader = tagReader
        GroupedSwitchExtractor = groupedSwitchExtractor
        AppSettingsParamIndex = appSettingsIndex
        HelpParam = helpParam
        CliParamIndex = cliIndex
    }

    current := result // assign result to children
    result

and preComputeUnionArgInfo<'Template when 'Template :> IArgParserTemplate> () = 
    preComputeUnionArgInfoInner [] None (fun () -> None) typeof<'Template>