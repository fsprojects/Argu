[<AutoOpen>]
module internal Argu.PreCompute

#nowarn "44"

open System
open System.Reflection
open System.Text.RegularExpressions

open FSharp.Reflection

[<AutoOpen>]
module private FastAttributes =
    let inline hasAttribute<'T when 'T :> Attribute> (attributes: obj[]) =
        attributes |> Array.exists (fun x -> x :? 'T)

    let inline hasAttribute2<'T when 'T :> Attribute> (attributes: obj[]) (declaringTypeAttributes: obj[]) =
        (hasAttribute<'T> attributes) || (hasAttribute<'T> declaringTypeAttributes)

    let inline tryGetAttribute<'T when 'T :> Attribute> (attributes: obj[]) =
        attributes |> Array.tryPick (function :? 'T as t -> Some t | _ -> None)

    let inline tryGetAttribute2<'T when 'T :> Attribute> (attributes: obj[]) (declaringTypeAttributes: obj[]) =
        match tryGetAttribute<'T> attributes with
        | Some _ as attr -> attr
        | None -> tryGetAttribute<'T> declaringTypeAttributes

let defaultHelpParam = "help"
let defaultHelpDescription = "display this list of options."

let getDefaultHelpParam (t : Type) =
    let prefixString =
        match t.TryGetAttribute<CliPrefixAttribute>() with
        | None -> CliPrefix.DoubleDash
        | Some pf -> pf.Prefix

    prefixString + defaultHelpParam

/// construct a CLI param from UCI name
let generateOptionName (uci : UnionCaseInfo) (attributes: obj[]) (declaringTypeAttributes: obj[])=
    let prefixString =
        match tryGetAttribute2<CliPrefixAttribute> attributes declaringTypeAttributes with
        | None -> CliPrefix.DoubleDash
        | Some pf -> pf.Prefix

    prefixString + uci.Name.ToLowerInvariant().Replace('_','-')

/// Generate a CLI Param for enumeration cases
let generateEnumName (name : string) = name.ToLowerInvariant().Replace('_','-')

/// construct an App.Config param from UCI name
let generateAppSettingsName (uci : UnionCaseInfo) =
    uci.Name.ToLowerInvariant().Replace('_',' ')

/// construct a command identifier from UCI name
let generateCommandName (uci : UnionCaseInfo) =
    uci.Name.ToUpperInvariant().Replace('_', ' ')

let private defaultLabelRegex = lazy(new Regex(@"^Item[0-9]*$", RegexOptions.Compiled))
/// Generates an argument label name from given PropertyInfo
let tryExtractUnionParameterLabel (p : PropertyInfo) =
    if defaultLabelRegex.Value.IsMatch p.Name then None
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

let private dict (s: ('key * 'value) []) =
    let result = System.Collections.Generic.Dictionary<'key, 'value>(s.Length)
    for pair in s do
        result.Add(fst pair, snd pair)
    result

let primitiveParsers = lazy(
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
        mkParser "bigint" System.Numerics.BigInteger.Parse string
        mkParser "guid" Guid string
        mkParser "base64" Convert.FromBase64String Convert.ToBase64String
    |])

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
            match tryGetAttribute<CustomCommandLineAttribute> (uci.GetCustomAttributes()) with
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
    let ok, f = primitiveParsers.Value.TryGetValue t
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
    lazy(
        let escapedChars = new String(validSeparatorChars) |> Regex.Escape
        new Regex(@"[" + escapedChars + "]+" , RegexOptions.Compiled))

let validateSeparator (uci : UnionCaseInfo) (sep : string) =
    if sep = null || not <| validSeparatorRegex.Value.IsMatch sep then
        let allowedchars = validSeparatorChars |> Seq.map (fun c -> String([|''';c;'''|])) |> String.concat ", "
        arguExn "parameter '%O' specifies invalid separator '%s' in CustomAssignment attribute.%sAllowed characters: %s"
            uci sep Environment.NewLine allowedchars

/// extracts the subcommand argument hierarchy for given UnionArgInfo
let getHierarchy (uai : UnionArgInfo) =
    let rec aux acc (uai : UnionArgInfo) =
        match uai.TryGetParent () with
        | None -> acc
        | Some ucai -> aux (ucai :: acc) (ucai.GetParent())

    aux [] uai

module Helpers =
    /// recognizes and extracts grouped switches
    /// e.g. -efx --> -e -f -x
    let groupedSwitchRegex (caseInfo: Lazy<UnionCaseArgInfo[]>) (inheritedParams: Lazy<UnionCaseArgInfo[]>) (helpParam: HelpParam) =
        lazy(
            let chars =
                caseInfo.Value
                |> Seq.append inheritedParams.Value
                |> Seq.collect (fun c -> c.CommandLineNames.Value)
                |> Seq.append helpParam.Flags
                |> Seq.filter (fun name -> name.Length = 2 && name.[0] = '-' && Char.IsLetterOrDigit name.[1])
                |> Seq.map (fun name -> name.[1])
                |> Seq.distinct
                |> Seq.toArray
                |> String

            if chars.Length = 0 then None else

            let regex = "^-[" + chars + "]+$"
            Some regex)

    let groupedSwitchExtractor (regexString: Lazy<string option>) =
        lazy(
            match regexString.Value with
            | None -> (fun _ -> [||])
            | Some regexString ->
                let regex = new Regex(regexString, RegexOptions.Compiled)
                (fun (arg : string) ->
                    if not <| regex.IsMatch arg then [||]
                    else Array.init (arg.Length - 1) (fun i -> String([|'-'; arg.[i + 1]|]))))

    let caseCtor uci = lazy(FSharpValue.PreComputeUnionConstructor(uci, allBindings))
    let fieldReader uci = lazy(FSharpValue.PreComputeUnionReader(uci, allBindings))

    let tupleConstructor (types: Type[]) =
        lazy(
            match types.Length with
            | 0 -> fun _ -> arguExn "internal error: attempting to call tuple constructor on nullary case."
            | 1 -> fun (o:obj[]) -> o.[0]
            | _ ->
                let tupleType = FSharpType.MakeTupleType types
                FSharpValue.PreComputeTupleConstructor tupleType)

    let assignParser (customAssignmentSeparator: Lazy<string option>) =
        lazy(
            match customAssignmentSeparator.Value with
            | None -> arguExn "internal error: attempting to call assign parser on invalid parameter."
            | Some sep ->
                let pattern = @"^(.+)" + (Regex.Escape sep) + "(.+)$"
                let regex = new Regex(pattern, RegexOptions.RightToLeft ||| RegexOptions.Compiled)
                fun token ->
                    let m = regex.Match token
                    if m.Success then Assignment(m.Groups.[1].Value, sep, m.Groups.[2].Value)
                    else NoAssignment)

/// generate argument parsing schema from given UnionCaseInfo
let rec private preComputeUnionCaseArgInfo (stack : Type list) (helpParam : HelpParam option)
                                            (getParent : unit -> UnionArgInfo)
                                            (uci : UnionCaseInfo) : UnionCaseArgInfo =

    let fields = uci.GetFields()
    let types = fields |> Array.map (fun f -> f.PropertyType)

    let caseCtor = Helpers.caseCtor uci

    // create a dummy instance for given union case
    let dummy = lazy(
        let dummyFields = types |> Array.map Unchecked.UntypedDefaultOf
        caseCtor.Value dummyFields :?> IArgParserTemplate)

    // use ref cell for late binding of parent argInfo
    let current = ref None
    let tryGetCurrent = fun () -> !current

    let attributes = lazy(uci.GetCustomAttributes())
    let declaringTypeAttributes = lazy(uci.DeclaringType.GetCustomAttributes(true))

    let isNoCommandLine = lazy(hasAttribute2<NoCommandLineAttribute> attributes.Value declaringTypeAttributes.Value)
    let isAppSettingsCSV = lazy(hasAttribute<ParseCSVAttribute> attributes.Value)
    let isExactlyOnce = lazy(hasAttribute2<ExactlyOnceAttribute> attributes.Value declaringTypeAttributes.Value)
    let isMandatory = lazy(isExactlyOnce.Value || hasAttribute2<MandatoryAttribute> attributes.Value declaringTypeAttributes.Value)
    let isUnique = lazy(isExactlyOnce.Value || hasAttribute2<UniqueAttribute> attributes.Value declaringTypeAttributes.Value)
    let isInherited = lazy(hasAttribute<InheritAttribute> attributes.Value)
    let isGatherAll = lazy(hasAttribute<GatherAllSourcesAttribute> attributes.Value)
    let isRest = lazy(hasAttribute<RestAttribute> attributes.Value)
    let isHidden = lazy(hasAttribute<HiddenAttribute> attributes.Value)

    let mainCommandName = lazy(
        match tryGetAttribute<MainCommandAttribute> attributes.Value with
        | None -> None
        | Some _ when isNoCommandLine.Value -> arguExn "parameter '%O' contains conflicting attributes 'MainCommand' and 'NoCommandLine'." uci
        | Some _ when types.Length = 0 -> arguExn "parameter '%O' contains MainCommand attribute but has unsupported arity 0." uci
        | Some attr ->
            match attr.ArgumentName with
            | null -> generateCommandName uci
            | name -> name
            |> Some)

    let isMainCommand = lazy(Option.isSome mainCommandName.Value)

    let cliPosition = lazy(
        match tryGetAttribute<CliPositionAttribute> attributes.Value with
        | Some attr ->
            match attr.Position with
            | CliPosition.Unspecified
            | CliPosition.First
            | CliPosition.Last as p -> p
            | _ -> arguExn "Invalid CliPosition setting '%O' for parameter '%O'" attr.Position uci
        | None -> CliPosition.Unspecified)

    let customAssignmentSeparator = lazy(
        match tryGetAttribute2<CustomAssignmentAttribute> attributes.Value declaringTypeAttributes.Value with
        | Some attr ->
            if isMainCommand.Value && types.Length = 1 then
                arguExn "parameter '%O' of arity 1 contains incompatible attributes 'CustomAssignment' and 'MainCommand'." uci
            if types.Length <> 1 && types.Length <> 2 then
                arguExn "parameter '%O' has CustomAssignment attribute but specifies %d parameters. Should be 1 or 2." uci types.Length
            elif isRest.Value then
                arguExn "parameter '%O' contains incompatible attributes 'CustomAssignment' and 'Rest'." uci

            validateSeparator uci attr.Separator
            Some attr.Separator

        | None -> None)

    let isGatherUnrecognized = lazy(
        if hasAttribute<GatherUnrecognizedAttribute> attributes.Value then
            match types with
            | _ when isMainCommand.Value -> arguExn "parameter '%O' contains incompatible combination of attributes 'MainCommand' and 'GatherUnrecognized'." uci
            | [|t|] when t = typeof<string> -> true
            | _ -> arguExn "parameter '%O' has GatherUnrecognized attribute but specifies invalid parameters. Must contain single parameter of type string." uci
        else
            false)

    let appSettingsSeparators, appSettingsSplitOptions =
        match tryGetAttribute2<AppSettingsSeparatorAttribute> attributes.Value declaringTypeAttributes.Value with
        | None -> [|","|], StringSplitOptions.None
        | Some attr when attr.Separators.Length = 0 ->
            arguExn "parameter '%O' specifies a null or empty AppSettings separator." uci

        | Some attr ->
            for sep in attr.Separators do
                if String.IsNullOrEmpty sep then
                    arguExn "parameter '%O' specifies a null or empty AppSettings separator." uci

            attr.Separators, attr.SplitOptions

    let argType =
        match types with
        | [|NestedParseResults _|] -> ArgumentType.SubCommand
        | [|Optional _|] -> ArgumentType.Optional
        | [|List _|] -> ArgumentType.List
        | _ -> ArgumentType.Primitive

    let parsers = lazy(
        match types with
        | [|NestedParseResults prt|] ->
            if Option.isSome customAssignmentSeparator.Value then
                arguExn "CustomAssignment in '%O' not supported in subcommands." uci
            if isRest.Value then
                arguExn "Rest attribute in '%O' not supported in subcommands." uci
            if isMandatory.Value then
                arguExn "Mandatory attribute in '%O' not supported in subcommands." uci
            if isMainCommand.Value then
                arguExn "MainCommand attribute in '%O' not supported in subcommands." uci
            if isInherited.Value then
                arguExn "Inherit attribute in '%O' not supported in subcommands." uci

            let argInfo = preComputeUnionArgInfoInner stack helpParam tryGetCurrent prt
            let shape = ShapeArgumentTemplate.FromType prt
            SubCommand(shape, argInfo, tryExtractUnionParameterLabel fields.[0])

        | [|Optional t|] ->
            if isRest.Value then
                arguExn "Rest attribute in '%O' not supported in optional parameters." uci

            if isMainCommand.Value then
                arguExn "MainCommand attribute in '%O' not supported in optional parameters." uci

            let label = tryExtractUnionParameterLabel fields.[0]

            OptionalParam(Existential.FromType t, getPrimitiveParserByType label t)

        | [|List t|] ->
            if Option.isSome customAssignmentSeparator.Value then
                arguExn "CustomAssignment in '%O' not supported for list parameters." uci

            if isRest.Value then
                arguExn "Rest attribute in '%O' not supported for list parameters." uci

            let label = tryExtractUnionParameterLabel fields.[0]

            ListParam(Existential.FromType t, getPrimitiveParserByType label t)

        | _ ->
            let getParser (p : PropertyInfo) =
                let label = tryExtractUnionParameterLabel p
                getPrimitiveParserByType label p.PropertyType

            Array.map getParser fields |> Primitives)

    let commandLineArgs = lazy(
        if isMainCommand.Value || isNoCommandLine.Value then []
        else
            let cliNames = [
                match tryGetAttribute<CustomCommandLineAttribute> attributes.Value with
                | None -> yield generateOptionName uci attributes.Value declaringTypeAttributes.Value
                | Some attr -> yield attr.Name ; yield! attr.AltNames

                yield!
                    attributes.Value
                    |> Array.filter (fun x -> x :? AltCommandLineAttribute)
                    |> Array.collect(fun x -> (x :?> AltCommandLineAttribute).Names)
            ]

            for name in cliNames do validateCliParam name

            cliNames)

    let appSettingsName = lazy(
        if hasAttribute2<NoAppSettingsAttribute> attributes.Value declaringTypeAttributes.Value then None
        else
            match tryGetAttribute<CustomAppSettingsAttribute> attributes.Value with
            | None -> Some <| generateAppSettingsName uci
            | Some _ when parsers.Value.Type = ArgumentType.SubCommand -> arguExn "CustomAppSettings in %O not supported in subcommands." uci
            | Some attr when not <| String.IsNullOrWhiteSpace attr.Name -> Some attr.Name
            | Some attr -> arguExn "AppSettings parameter '%s' contains invalid characters." attr.Name)

    /// gets the default name of the argument
    let defaultName = lazy(
        match commandLineArgs.Value with
        | h :: _ -> h
        | [] when isMainCommand.Value ->
            match parsers.Value with
            | Primitives ps ->
                let name = ps |> Seq.map (fun p -> "<" + p.Description + ">" ) |> String.concat " "
                if isRest.Value then name + "..." else name
            | ListParam(_,p) -> "<" + p.Description + ">..."
            | _ -> arguExn "internal error in argu parser representation %O." uci
        | _ when Option.isSome appSettingsName.Value -> appSettingsName.Value.Value
        | _ -> arguExn "parameter '%O' needs to have at least one parse source." uci)

    let fieldReader = Helpers.fieldReader uci
    let fieldCtor = Helpers.tupleConstructor types
    let assignParser = Helpers.assignParser customAssignmentSeparator

    if isAppSettingsCSV.Value && fields.Length <> 1 then
        arguExn "CSV attribute is only compatible with branches of unary fields."

    // extract the description string for given union case
    let description = lazy(
        try dummy.Value.Usage
        with _ -> arguExn "Error generating usage string from IArgParserTemplate for case %O." uci)

    let uai = {
        Tag = uci.Tag
        UnionCaseInfo = lazy(uci)
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
        ArgumentType = argType
        AppSettingsCSV = isAppSettingsCSV
        MainCommandName = mainCommandName
        IsMandatory = isMandatory
        IsUnique = isUnique
        IsInherited = isInherited
        GatherAllSources = isGatherAll
        IsRest = isRest
        CliPosition = cliPosition
        CustomAssignmentSeparator = customAssignmentSeparator
        AssignmentParser = assignParser
        IsGatherUnrecognized = isGatherUnrecognized
        IsHidden = isHidden
    }

    current := Some uai // assign result to children
    uai

and private preComputeUnionArgInfoInner (stack : Type list) (helpParam : HelpParam option) (tryGetParent : unit -> UnionCaseArgInfo option) (t : Type) : UnionArgInfo =
    if not <| FSharpType.IsUnion(t, allBindings) then
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

    let caseInfo = lazy(
        FSharpType.GetUnionCases(t, allBindings)
        |> Seq.map (preComputeUnionCaseArgInfo (t :: stack) (Some helpParam) getCurrent)
        |> Seq.sortBy (fun a -> a.Tag)
        |> Seq.toArray)

    let containsSubcommands = lazy(caseInfo.Value |> Array.exists (fun c -> c.ArgumentType = ArgumentType.SubCommand))
    let isRequiredSubcommand = lazy(t.ContainsAttribute<RequireSubcommandAttribute>() && containsSubcommands.Value)

    // need to delay this computation since it depends
    // on completed process of any potential parents
    let inheritedParams = lazy(
        match tryGetParent() with
        | None -> [||]
        | Some parent ->
            let pInfo = parent.GetParent()
            pInfo.Cases.Value
            |> Seq.filter (fun cI -> cI.IsInherited.Value)
            |> Seq.append pInfo.InheritedParams.Value
            |> Seq.toArray)

    let tagReader = lazy(FSharpValue.PreComputeUnionTagReader(t, allBindings))

    let cliIndex = lazy(
        caseInfo.Value
        |> Seq.append inheritedParams.Value
        |> Seq.collect (fun cs -> cs.CommandLineNames.Value |> Seq.map (fun name -> name, cs))
        |> PrefixDictionary)

    let appSettingsIndex = lazy(
        caseInfo.Value
        |> Array.choose (fun cs -> match cs.AppSettingsName.Value with Some name -> Some(name, cs) | None -> None)
        |> dict)

    let unrecognizedParam = lazy(
        match caseInfo.Value |> Array.filter (fun cI -> cI.IsGatherUnrecognized.Value) with
        | [||] -> None
        | [|ur|] -> Some ur
        | _ -> arguExn "template type '%O' has specified the GatherUnrecognized attribute in more than one union cases." t)

    let mainCommandParam = lazy(
        match caseInfo.Value |> Array.filter (fun cI -> cI.IsMainCommand) with
        | [||] -> None
        | [|mcp|] -> Some mcp
        | _ -> arguExn "template type '%O' has specified the MainCommand attribute in more than one union cases." t)

    let groupedSwitchRegex = Helpers.groupedSwitchRegex caseInfo inheritedParams helpParam

    let result = {
        Type = lazy(t)
        Depth = List.length stack
        TryGetParent = tryGetParent
        Cases = caseInfo
        TagReader = tagReader
        HelpParam = helpParam
        ContainsSubcommands = containsSubcommands
        IsRequiredSubcommand = isRequiredSubcommand
        GroupedSwitchRegex = groupedSwitchRegex
        GroupedSwitchExtractor = Helpers.groupedSwitchExtractor groupedSwitchRegex
        AppSettingsParamIndex = appSettingsIndex
        InheritedParams = inheritedParams
        CliParamIndex = cliIndex
        UnrecognizedGatherParam = unrecognizedParam
        MainCommandParam = mainCommandParam
    }

    current := result // assign result to children
    result

and preComputeUnionArgInfo<'Template when 'Template :> IArgParserTemplate> () =
    preComputeUnionArgInfoInner [] None (fun () -> None) typeof<'Template>

// used for performing additional checks on the completed dependency graph
let checkUnionArgInfo (result: UnionArgInfo) =
    let rec postProcess (argInfo : UnionArgInfo) =
        // check for conflicting CLI identifiers
        argInfo.Cases.Value
        |> Seq.append argInfo.InheritedParams.Value // this will only have been populated post-construction
        |> Seq.collect (fun arg -> arg.CommandLineNames.Value |> Seq.map (fun cliName -> cliName, arg))
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
            argInfo.Cases.Value
            |> Seq.choose(fun arg -> arg.AppSettingsName.Value |> Option.map (fun name -> name, arg))
            |> Seq.groupBy fst
            |> Seq.tryFind(fun (_,args) -> Seq.length args > 1)
            |> Option.iter (fun (name,args) ->
                let conflicts = args |> Seq.map snd |> Seq.toArray
                arguExn "parameters '%O' and '%O' using conflicting AppSettings identifier '%s'."
                    conflicts.[0].UnionCaseInfo conflicts.[1].UnionCaseInfo name)

        // Evaluate every lazy property to ensure that their checks run
        for case in argInfo.Cases.Value do
            case.Name.Value |> ignore
            case.CommandLineNames.Value |> ignore
            case.AppSettingsName.Value |> ignore
            case.ParameterInfo.Value |> ignore
            case.AppSettingsCSV.Value |> ignore
            case.MainCommandName.Value |> ignore
            case.IsMandatory.Value |> ignore
            case.IsUnique.Value |> ignore
            case.IsInherited.Value |> ignore
            case.GatherAllSources.Value |> ignore
            case.IsRest.Value |> ignore
            case.CliPosition.Value |> ignore
            case.CustomAssignmentSeparator.Value |> ignore
            case.IsGatherUnrecognized.Value |> ignore
            case.IsHidden.Value |> ignore
            case.CaseCtor.Value |> ignore
            case.Description.Value |> ignore

        // iterate through the child nodes
        for case in argInfo.Cases.Value do
            match case.ParameterInfo.Value with
            | SubCommand(_, aI, _) -> postProcess aI
            | _ -> ()

    postProcess result
    result

