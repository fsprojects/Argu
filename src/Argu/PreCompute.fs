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

/// Generate a CLI Param for enumeration cases
let generateEnumName (name : string) = name.ToLower().Replace('_','-')

/// construct an App.Config param from UCI name
let generateAppSettingsName (uci : UnionCaseInfo) =
    uci.Name.ToLower().Replace('_',' ')

let private defaultLabelRegex = new Regex(@"^Item[0-9]*$", RegexOptions.Compiled)
/// Generates an argument label name from given PropertyInfo
let tryExtractUnionParameterLabel (p : PropertyInfo) =
    if defaultLabelRegex.IsMatch p.Name then None
    else Some(p.Name.Replace('_',' '))

let (|NestedParseResults|Optional|List|Other|) (t : Type) =
    if t.IsGenericType then
        let gt = t.GetGenericTypeDefinition()
        if typeof<IParseResult>.IsAssignableFrom t then NestedParseResults(t.GetGenericArguments().[0])
        elif gt = typedefof<_ option> then Optional(t.GetGenericArguments().[0])
        elif gt = typedefof<_ list> then List(t.GetGenericArguments().[0])
        else Other
    else Other

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
        mkParser "double" Double.Parse string
        mkParser "decimal" Decimal.Parse string
#if !NET35
        mkParser "bigint" System.Numerics.BigInteger.Parse string
#endif
        mkParser "guid" Guid string
        mkParser "base64" Convert.FromBase64String Convert.ToBase64String
    |]

/// Creates a primitive parser from an enumeration
let tryGetEnumerationParser label (t : Type) =
    if not t.IsEnum then None else
    let names = Enum.GetNames(t) |> Seq.map generateEnumName
    let values = Enum.GetValues(t) |> Seq.cast<obj>
    let index = Seq.zip names values |> Seq.toArray
    let name = names |> String.concat "|"

    let parser (text : string) =
        let text = text.Trim()
        let _,value = index |> Array.find (fun (id,_) -> text = id)
        value

    let unparser (value : obj) =
        match Enum.GetName(t, value) with
        | null -> failwith "invalid enum value!"
        | name -> generateEnumName name

    Some {
        Name = name
        Label = label
        Type = t
        Parser = parser
        UnParser = unparser
    }

/// Creates a primitive parser from an F# DU enumeration 
/// (i.e. one with no parameters in any of its union cases)
let tryGetDuEnumerationParser label (t : Type) =
    if not <| FSharpType.IsUnion(t, allBindings) then None else

    let ucis = FSharpType.GetUnionCases(t, allBindings)
    if ucis |> Array.exists (fun uci -> uci.GetFields().Length > 0) then None else

    let tagReader = lazy(FSharpValue.PreComputeUnionTagReader(t, allBindings))
    let extractUciInfo (uci : UnionCaseInfo) =
        let name =
            match uci.TryGetAttribute<CustomCommandLineAttribute>() with
            | None -> generateEnumName uci.Name
            | Some attr -> attr.Name

        let value = FSharpValue.MakeUnion(uci, [||], allBindings)
        name, value

    let index = ucis |> Array.map extractUciInfo
    let name = index |> Seq.map fst |> String.concat "|"

    let parser (text : string) =
        let text = text.Trim()
        let _,value = index |> Array.find (fun (id,_) -> text = id)
        value

    let unparser (value : obj) =
        let tag = tagReader.Value value
        let id,_ = index.[tag]
        id

    Some {
        Name = name
        Label = label
        Type = t
        Parser = parser
        UnParser = unparser
    }

let getPrimitiveParserByType label (t : Type) = 
    let ok, f = primitiveParsers.TryGetValue t
    if ok then f label
    else
        match tryGetEnumerationParser label t with
        | Some p -> p
        | None ->

        match tryGetDuEnumerationParser label t with
        | Some p -> p
        | None ->


        // refine error messaging depending on the input time
        match t with
        | NestedParseResults _ -> arguExn "Nested ParseResult<'T> parameters can only occur as standalone parameters in union constructors."
        | Optional _ -> arguExn "F# Option parameters can only occur as standalone parameters in union constructors."
        | List _ -> arguExn "F# List parameters can only occur as standalone parameters in union constructors."
        | _ -> arguExn "template contains unsupported field of type '%O'." t

let private validCliParamRegex = new Regex(@"\S+", RegexOptions.Compiled)
let validateCliParam (name : string) =
    if name = null || not <| validCliParamRegex.IsMatch name then
        arguExn "CLI parameter '%s' contains invalid characters." name

let validSeparatorChars = [|'=' ; ':' ; '.' ; '#' ; '+' ; '^' ; '&' ; '?' ; '%' ; '$' ; '~' ; '@'|]
let private validSeparatorRegex = 
    let escapedChars = new String(validSeparatorChars) |> Regex.Escape
    new Regex(sprintf @"[%s]+" escapedChars, RegexOptions.Compiled)

let validateSeparator (uci : UnionCaseInfo) (sep : string) =   
    if sep = null || not <| validSeparatorRegex.IsMatch sep then
        let allowedchars = validSeparatorChars |> Seq.map (fun c -> sprintf "'%c'" c) |> String.concat ", "
        arguExn "parameter '%O' specifies invalid separator '%s' in CustomAssignment attribute.%sAllowed characters: %s" 
            uci sep Environment.NewLine allowedchars

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

    // create a dummy instance for given union case
    let dummyFields = types |> Array.map Unchecked.UntypedDefaultOf
    let dummy = caseCtor dummyFields :?> IArgParserTemplate

    // use ref cell for late binding of parent argInfo
    let current = ref None
    let tryGetCurrent = fun () -> !current

    let isFirst = uci.ContainsAttribute<FirstAttribute> ()
    let isAppSettingsCSV = uci.ContainsAttribute<ParseCSVAttribute> ()
    let isExactlyOnce = uci.ContainsAttribute<ExactlyOnceAttribute> (true)
    let isMandatory = isExactlyOnce || uci.ContainsAttribute<MandatoryAttribute> (true)
    let isUnique = isExactlyOnce || uci.ContainsAttribute<UniqueAttribute> (true)
    let isInherited = uci.ContainsAttribute<InheritAttribute> ()
    let isGatherAll = uci.ContainsAttribute<GatherAllSourcesAttribute> ()
    let isRest = uci.ContainsAttribute<RestAttribute> ()
    let isHidden = uci.ContainsAttribute<HiddenAttribute> ()
    let customAssignmentSeparator = 
        match uci.TryGetAttribute<CustomAssignmentAttribute> (true) with
        | Some attr ->
            if types.Length <> 1 && types.Length <> 2 then
                arguExn "parameter '%O' has CustomAssignment attribute but specifies %d parameters. Should be 1 or 2." uci types.Length
            elif isRest then
                arguExn "parameter '%O' contains incompatible attributes 'CustomAssignment' and 'Rest'." uci

            validateSeparator uci attr.Separator
            Some attr.Separator

        | None -> None

    let isGatherUnrecognized =
        if uci.ContainsAttribute<GatherUnrecognized>() then
            match types with
            | [|t|] when t = typeof<string> -> true
            | _ -> arguExn "parameter '%O' has GatherUnrecognized attribute but specifies invalid parameters. Must contain single parameter of type string." uci
        else
            false

    let appSettingsSeparators, appSettingsSplitOptions =
        match uci.TryGetAttribute<AppSettingsSeparatorAttribute> (true) with
        | None -> [|","|], StringSplitOptions.None
        | Some attr when attr.Separators.Length = 0 ->
            arguExn "parameter '%O' specifies a null or empty AppSettings separator." uci

        | Some attr ->
            for sep in attr.Separators do
                if String.IsNullOrEmpty sep then
                    arguExn "parameter '%O' specifies a null or empty AppSettings separator." uci

            attr.Separators, attr.SplitOptions

    let parsers =
        match types with
        | [|NestedParseResults prt|] -> 
            if Option.isSome customAssignmentSeparator then
                arguExn "CustomAssignment in '%O' not supported in subcommands." uci
            if isRest then
                arguExn "Rest attribute in '%O' not supported in subcommands." uci
            if isMandatory then
                arguExn "Mandatory attribute in '%O' not supported in subcommands." uci
            if isInherited then
                arguExn "Inherit attribute in '%O' not supported in subcommands." uci

            let argInfo = preComputeUnionArgInfoInner stack helpParam tryGetCurrent prt 
            let shape = ShapeArgumentTemplate.FromType prt
            SubCommand(shape, argInfo, tryExtractUnionParameterLabel fields.[0])

        | [|Optional t|] ->
            if isRest then
                arguExn "Rest attribute in '%O' not supported for optional parameters." uci

            let label = tryExtractUnionParameterLabel fields.[0]

            OptionalParam(Existential.FromType t, getPrimitiveParserByType label t)

        | [|List t|] ->
            if Option.isSome customAssignmentSeparator then
                arguExn "CustomAssignment in '%O' not supported for list parameters." uci

            if isRest then
                arguExn "Rest attribute in '%O' not supported for list parameters." uci

            let label = tryExtractUnionParameterLabel fields.[0]

            ListParam(Existential.FromType t, getPrimitiveParserByType label t)
            
        | _ -> 
            let getParser (p : PropertyInfo) =
                let label = tryExtractUnionParameterLabel p
                getPrimitiveParserByType label p.PropertyType

            Array.map getParser fields |> Primitives

    let commandLineArgs =
        if uci.ContainsAttribute<NoCommandLineAttribute> (true) then
            match parsers with
            | SubCommand _ -> arguExn "NoCommandLine attribute in '%O' not supported in subcommands." uci
            | _ -> []
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
            | Some _ when parsers.Type = ArgumentType.SubCommand -> arguExn "CustomAppSettings in %O not supported in subcommands." uci
            | Some attr when not <| isNullOrWhiteSpace attr.Name -> Some attr.Name
            | Some attr -> arguExn "AppSettings parameter '%s' contains invalid characters." attr.Name

    /// gets the default name of the argument
    let defaultName =
        match commandLineArgs with
        | h :: _ -> h
        | _ when Option.isSome appSettingsName -> appSettingsName.Value
        | _ -> arguExn "parameter '%O' needs to have at least one parse source." uci

    let fieldCtor = lazy(
        match types.Length with
        | 0 -> fun _ -> arguExn "internal error: attempting to call tuple constructor on nullary case."
        | 1 -> fun (o:obj[]) -> o.[0]
        | _ ->
            let tupleType = FSharpType.MakeTupleType types
            FSharpValue.PreComputeTupleConstructor tupleType)

    let fieldReader = lazy(FSharpValue.PreComputeUnionReader(uci, bindingFlags = allBindings))

    let assignParser = lazy(
        match customAssignmentSeparator with
        | None -> arguExn "internal error: attempting to call assign parser on invalid parameter."
        | Some sep ->
            let regex = new Regex(sprintf @"^(.+)%s(.+)$" (Regex.Escape sep), RegexOptions.Compiled)
            fun token ->
                let m = regex.Match token
                if m.Success then Assignment(m.Groups.[1].Value, sep, m.Groups.[2].Value)
                else NoAssignment)

    if isAppSettingsCSV && fields.Length <> 1 then 
        arguExn "CSV attribute is only compatible with branches of unary fields." 

    // extract the description string for given union case
    let description = 
        try dummy.Usage.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        with _ -> 
            arguExn "Error generating usage string from IArgParserTemplate for case %O." uci

    if List.isEmpty description then
        arguExn "Usage string for case '%O' was empty." uci

    let uai = {
        UnionCaseInfo = uci
        Arity = fields.Length
        Depth = List.length stack - 1
        CaseCtor = caseCtor
        FieldReader = fieldReader
        FieldCtor = fieldCtor
        Name = defaultName
        GetParent = getParent
        CommandLineNames = commandLineArgs
        AppSettingsName = appSettingsName
        AppSettingsSeparators = appSettingsSeparators
        AppSettingsSplitOptions = appSettingsSplitOptions
        Description = description
        ParameterInfo = parsers
        AppSettingsCSV = isAppSettingsCSV
        IsMandatory = isMandatory
        IsUnique = isUnique
        IsInherited = isInherited
        GatherAllSources = isGatherAll
        IsRest = isRest
        IsFirst = isFirst
        CustomAssignmentSeparator = customAssignmentSeparator
        AssignmentParser = assignParser
        IsGatherUnrecognized = isGatherUnrecognized
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

    let containsSubcommands = caseInfo |> Array.exists (fun c -> c.Type = ArgumentType.SubCommand)
    let isRequiredSubcommand = containsSubcommands && t.ContainsAttribute<RequireSubcommandAttribute>()

    // need to delay this computation since it depends
    // on completed process of any potential parents
    let inheritedParams = lazy(
        match tryGetParent() with
        | None -> [||]
        | Some parent ->
            let pInfo = parent.GetParent()
            pInfo.Cases
            |> Seq.filter (fun cI -> cI.IsInherited)
            |> Seq.append pInfo.InheritedParams.Value
            |> Seq.toArray)

    // recognizes and extracts grouped switches
    // e.g. -efx --> -e -f -x
    let groupedSwitchExtractor = lazy(
        let chars =
            caseInfo
            |> Seq.append inheritedParams.Value
            |> Seq.collect (fun c -> c.CommandLineNames)
            |> Seq.append helpParam.Flags
            |> Seq.filter (fun name -> name.Length = 2 && name.[0] = '-' && Char.IsLetterOrDigit name.[1])
            |> Seq.map (fun name -> name.[1])
            |> Seq.distinct
            |> Seq.toArray
            |> String

        if chars.Length = 0 then (fun _ -> [||]) else

        let regex = new Regex(sprintf "^-[%s]+$" chars, RegexOptions.Compiled)

        fun (arg : string) ->
            if not <| regex.IsMatch arg then [||] 
            else Array.init (arg.Length - 1) (fun i -> sprintf "-%c" arg.[i + 1]))

    let tagReader = lazy(FSharpValue.PreComputeUnionTagReader(t, bindingFlags = allBindings))

    let cliIndex = lazy(
        caseInfo
        |> Seq.append inheritedParams.Value
        |> Seq.collect (fun cs -> cs.CommandLineNames |> Seq.map (fun name -> name, cs))
        |> PrefixDictionary)

    let appSettingsIndex = lazy(
        caseInfo
        |> Seq.choose (fun cs -> match cs.AppSettingsName with Some name -> Some(name, cs) | None -> None)
        |> dict)

    let unrecognizedParam =
        match caseInfo |> Array.filter (fun cI -> cI.IsGatherUnrecognized) with
        | [||] -> None
        | [|ur|] -> Some ur
        | _ -> arguExn "template type '%O' has specified the GatherUnrecognized attribute in more than one union cases." t

    let result = {
        Type = t
        Depth = List.length stack
        TryGetParent = tryGetParent
        Cases = caseInfo
        TagReader = tagReader
        HelpParam = helpParam
        ContainsSubcommands = containsSubcommands
        IsRequiredSubcommand = isRequiredSubcommand
        GroupedSwitchExtractor = groupedSwitchExtractor
        AppSettingsParamIndex = appSettingsIndex
        InheritedParams = inheritedParams
        CliParamIndex = cliIndex
        UnrecognizedGatherParam = unrecognizedParam
    }

    current := result // assign result to children
    result

and preComputeUnionArgInfo<'Template when 'Template :> IArgParserTemplate> () = 
    let result = preComputeUnionArgInfoInner [] None (fun () -> None) typeof<'Template>

    // used for performing additional checks on the completed dependency graph
    let rec postProcess (argInfo : UnionArgInfo) =
        // check for conflicting CLI identifiers
        argInfo.Cases
        |> Seq.append argInfo.InheritedParams.Value // this will only have been populated post-construction
        |> Seq.collect (fun arg -> arg.CommandLineNames |> Seq.map (fun cliName -> cliName, arg))
        |> Seq.map (fun ((name, arg) as t) ->
            if argInfo.HelpParam.IsHelpFlag name then
                arguExn "parameter '%O' using CLI identifier '%s' which is reserved for help parameters." arg.UnionCaseInfo name
            t)
        |> Seq.groupBy fst
        |> Seq.tryFind (fun (_,args) -> Seq.length args > 1)
        |> Option.iter (fun (name,args) ->
            let conflicts = args |> Seq.map snd |> Seq.toArray
            arguExn "parameters '%O' and '%O' using conflicting CLI identifier '%s'." 
                conflicts.[0].UnionCaseInfo conflicts.[1].UnionCaseInfo name)

        // check for conflicting AppSettings identifiers
        if argInfo.Depth = 0 then
            argInfo.Cases
            |> Seq.choose(fun arg -> arg.AppSettingsName |> Option.map (fun name -> name, arg))
            |> Seq.groupBy fst
            |> Seq.tryFind(fun (_,args) -> Seq.length args > 1)
            |> Option.iter (fun (name,args) ->
                let conflicts = args |> Seq.map snd |> Seq.toArray
                arguExn "parameters '%O' and '%O' using conflicting AppSettings identifier '%s'." 
                    conflicts.[0].UnionCaseInfo conflicts.[1].UnionCaseInfo name)

        // iterate through the child nodes
        for case in argInfo.Cases do
            match case.ParameterInfo with
            | SubCommand(_, aI, _) -> postProcess aI
            | _ -> ()

    postProcess result
    result