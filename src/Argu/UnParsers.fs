[<AutoOpen>]
module internal Argu.UnParsers

open System
open System.Text
open System.Xml
open System.Xml.Linq

open FSharp.Reflection

/// Number of spaces to be inserted before a cli switch name in the usage string
let [<Literal>] switchOffset = 4
/// Number of spaces to be inserted before a cli switch description text
let [<Literal>] descriptionOffset = 26

/// <summary>
///     print the command line syntax
/// </summary>
let mkCommandLineSyntax (argInfo : UnionArgInfo) (prefix : string) (maxWidth : int) (programName : string) = stringExpr {
    do if maxWidth < 1 then raise <| new ArgumentOutOfRangeException("maxWidth", "must be positive value.")
    let! length0 = StringExpr.currentLength
    yield prefix
    yield programName

    for command in getHierarchy argInfo do
        yield ' '
        yield command.Name.Value

    let! length1 = StringExpr.currentLength
    let offset = length1 - length0

    let insertToken =
        let startOfCurrentLine = ref length0
        let isFirstToken = ref true
        fun (token:string) -> stringExpr {
            let! currLength = StringExpr.currentLength
            let currLineLength = currLength - !startOfCurrentLine
            if not !isFirstToken && currLineLength + token.Length > maxWidth then
                yield Environment.NewLine
                yield! StringExpr.whiteSpace offset
                startOfCurrentLine := currLength + Environment.NewLine.Length
            yield token
            isFirstToken := false
        }

    let printedCases =
        argInfo.Cases.Value
        |> Seq.filter (fun aI -> not aI.IsHidden.Value && not aI.IsMainCommand)
        |> Seq.filter (fun aI -> aI.ArgumentType <> ArgumentType.SubCommand)
        |> Seq.sortBy (fun aI -> aI.CliPosition.Value)

    match argInfo.HelpParam.Flags with
    | h :: _ -> yield! insertToken (sprintf " [%s]" h)
    | _ -> ()

    for aI in printedCases do
        match aI.CommandLineNames.Value with
        | [] -> ()
        | name :: _ ->

        let format() = stringExpr {
            yield ' '
            if not aI.IsMandatory.Value then yield '['
            yield name

            match aI.ParameterInfo.Value with
            | Primitives parsers ->
                match aI.CustomAssignmentSeparator.Value with
                | Some sep when parsers.Length = 1 ->
                    yield sprintf "%s<%s>" sep parsers.[0].Description
                | Some sep ->
                    assert(parsers.Length = 2)
                    yield sprintf " <%s>%s<%s>" parsers.[0].Description sep parsers.[1].Description
                | None ->
                    for p in parsers do
                        yield sprintf " <%s>" p.Description

                if aI.IsRest.Value then yield "..."

            | OptionalParam (_,parser) ->
                match aI.CustomAssignmentSeparator.Value with
                | Some sep -> yield sprintf "[%s<%s>]" sep parser.Description
                | None -> yield sprintf " [<%s>]" parser.Description

            | SubCommand (label = None) -> yield " <options>"
            | SubCommand (label = Some label) -> yield sprintf " <%s>" label
            | ListParam (_,parser) -> yield sprintf " [<%s>...]" parser.Description

            if not aI.IsMandatory.Value then yield ']'
        }

        let formatCase = format() |> StringExpr.build
        yield! insertToken formatCase

    if argInfo.ContainsSubcommands.Value then
        let subCommandString =
            stringExpr {
                yield ' '
                if not argInfo.IsRequiredSubcommand.Value then yield '['
                yield "<subcommand> [<options>]"
                if not argInfo.IsRequiredSubcommand.Value then yield ']'
            } |> StringExpr.build

        yield! insertToken subCommandString

    match argInfo.MainCommandParam.Value with
    | None -> ()
    | Some mc ->
        let formatMainCommand() = stringExpr {
            yield ' '
            if not mc.IsMandatory.Value then yield '['
            match mc.ParameterInfo.Value with
            | Primitives parsers ->
                assert(parsers.Length > 0)
                yield sprintf "<%s>" parsers.[0].Description
                for i = 1 to parsers.Length - 1 do
                    yield sprintf " <%s>" parsers.[i].Description

            | ListParam(_, parser) ->
                yield sprintf "<%s>..." parser.Description

            | _ -> arguExn "internal error: MainCommand param has invalid internal representation."
            if not mc.IsMandatory.Value then yield ']'
        }

        let mainCommand = formatMainCommand() |> StringExpr.build
        yield! insertToken mainCommand
}

/// <summary>
///     print usage string for given arg info
/// </summary>
let mkArgUsage width (aI : UnionCaseArgInfo) = stringExpr {
    if not aI.IsCommandLineArg then () else
    let! start = StringExpr.currentLength
    yield! StringExpr.whiteSpace switchOffset
    yield String.concat ", " aI.CommandLineNames.Value

    match aI.ParameterInfo.Value with
    | Primitives parsers when aI.IsMainCommand ->
        assert(parsers.Length > 0)
        yield sprintf "<%s>" parsers.[0].Description
        for i = 1 to parsers.Length - 1 do
            yield sprintf " <%s>" parsers.[i].Description

        if aI.IsRest.Value then yield "..."

    | Primitives parsers ->
        match aI.CustomAssignmentSeparator.Value with
        | Some sep when parsers.Length = 1 ->
            yield sprintf "%s<%s>" sep parsers.[0].Description
        | Some sep ->
            assert (parsers.Length = 2)
            yield sprintf " <%s>%s<%s>" parsers.[0].Description sep parsers.[1].Description
        | None ->
            for p in parsers do
                yield sprintf " <%s>" p.Description

        if aI.IsRest.Value then yield "..."

    | OptionalParam (_,parser) ->
        match aI.CustomAssignmentSeparator.Value with
        | Some sep -> yield sprintf "[%s<%s>]" sep parser.Description
        | None -> yield sprintf " [<%s>]" parser.Description

    | ListParam (_,parser) when aI.IsMainCommand ->
        yield sprintf "<%s>..." parser.Description

    | ListParam (_,parser) ->
        yield sprintf " [<%s>...]" parser.Description

    | SubCommand(_,_,Some label) -> yield sprintf " <%s>" label
    | SubCommand(_,_,None) -> yield " <options>"

    let! finish = StringExpr.currentLength
    if finish - start >= descriptionOffset then
        yield Environment.NewLine
        yield! StringExpr.whiteSpace descriptionOffset
    else
        yield! StringExpr.whiteSpace (descriptionOffset - finish + start)

    let lines = wordwrap (max (width - descriptionOffset) 1) aI.Description.Value

    match lines with
    | [] -> ()
    | h :: tail ->
        yield h
        yield Environment.NewLine
        for t in tail do
            yield! StringExpr.whiteSpace descriptionOffset
            yield t
            yield Environment.NewLine
}

/// <summary>
///     print usage string for given help param
/// </summary>
let mkHelpParamUsage width (hp : HelpParam) = stringExpr {
    match hp.Flags with
    | [] -> ()
    | flags ->
        let! start = StringExpr.currentLength
        yield! StringExpr.whiteSpace switchOffset
        yield String.concat ", " flags

        let! finish = StringExpr.currentLength
        if finish - start > descriptionOffset then
            yield Environment.NewLine
            yield! StringExpr.whiteSpace descriptionOffset
        else
            yield! StringExpr.whiteSpace (descriptionOffset - finish + start)

        let lines = wordwrap (max (width - descriptionOffset) 1) hp.Description
        match lines with
        | [] -> ()
        | h :: tail ->
            yield h
            yield Environment.NewLine
            for t in tail do
                yield! StringExpr.whiteSpace descriptionOffset
                yield t
                yield Environment.NewLine
}

/// <summary>
///     print usage string for a collection of arg infos
/// </summary>
let mkUsageString (argInfo : UnionArgInfo) (programName : string) hideSyntax width (message : string option) = stringExpr {
    match message with
    | Some msg -> yield msg; yield Environment.NewLine
    | None -> ()

    if not hideSyntax then
        yield! mkCommandLineSyntax argInfo "USAGE: " width programName
        yield Environment.NewLine

    let options, subcommands =
        argInfo.Cases.Value
        |> Seq.filter (fun aI -> not aI.IsHidden.Value)
        |> Seq.filter (fun aI -> not aI.IsMainCommand)
        |> Seq.partition (fun aI -> aI.ArgumentType <> ArgumentType.SubCommand)

    match argInfo.MainCommandParam.Value with
    | Some aI when not aI.IsHidden.Value ->
        let! length = StringExpr.currentLength
        if length > 0 then yield Environment.NewLine
        assert(Option.isSome aI.MainCommandName.Value)
        yield sprintf "%s:" aI.MainCommandName.Value.Value
        yield Environment.NewLine; yield Environment.NewLine
        yield! mkArgUsage width aI
    | _ -> ()

    if subcommands.Length > 0 then
        let! length = StringExpr.currentLength
        if length > 0 then yield Environment.NewLine
        yield "SUBCOMMANDS:"
        yield Environment.NewLine; yield Environment.NewLine

        for aI in subcommands do yield! mkArgUsage width aI

        match argInfo.HelpParam.Flags with
        | [] -> ()
        | helpflag :: _ ->
            yield Environment.NewLine
            let wrappedList =
                sprintf "Use '%s <subcommand> %s' for additional information." programName helpflag
                |> wordwrap (max (width - switchOffset) 1)

            for line in wrappedList do
                yield String.mkWhiteSpace switchOffset
                yield line
                yield Environment.NewLine

    if options.Length > 0 || argInfo.UsesHelpParam then
        let! length = StringExpr.currentLength
        if length > 0 then yield Environment.NewLine
        yield "OPTIONS:"
        yield Environment.NewLine; yield Environment.NewLine

        for aI in options do yield! mkArgUsage width aI
        for aI in argInfo.InheritedParams.Value |> Seq.filter (fun a -> not a.IsHidden.Value) do yield! mkArgUsage width aI

        yield! mkHelpParamUsage width argInfo.HelpParam
}

/// <summary>
///     print a command line argument for a set of parameters
/// </summary>
let rec mkCommandLineArgs (argInfo : UnionArgInfo) (args : seq<obj>) =
    let mkEntry (aI : UnionCaseArgInfo) (t : obj) = seq {
        if not aI.IsCommandLineArg then () else

        let fields = aI.FieldReader.Value t

        let clName() = List.head aI.CommandLineNames.Value

        match aI.ParameterInfo.Value with
        | Primitives parsers ->
            let inline unpars i = parsers.[i].UnParser fields.[i]
            match aI.CustomAssignmentSeparator.Value with
            | Some sep when parsers.Length = 1 ->
                yield sprintf "%s%s%s" (clName()) sep (unpars 0)
            | Some sep ->
                assert(parsers.Length = 2)
                if not aI.IsMainCommand then yield clName()
                yield sprintf "%s%s%s" (unpars 0) sep (unpars 1)

            | None ->
                if not aI.IsMainCommand then yield clName()

                for i = 0 to fields.Length - 1 do
                    yield unpars i

        | OptionalParam(existential, parser) ->
            let optional =
                existential.Accept { new IFunc<obj option> with
                    member __.Invoke<'T> () = fields.[0] :?> 'T option |> Option.map box }

            match optional with
            | None -> yield clName()
            | Some v ->
                match aI.CustomAssignmentSeparator.Value with
                | Some sep -> yield sprintf "%s%s%s" (clName()) sep (parser.UnParser v)
                | None -> yield clName() ; yield parser.UnParser v

        | ListParam(_, parser) ->
            if not aI.IsMainCommand then yield clName()
            let objSeq = fields.[0] |> unbox<System.Collections.IEnumerable> |> Seq.cast<obj>
            for obj in objSeq do yield parser.UnParser obj

        | SubCommand (_, nested, _) ->
            if not aI.IsMainCommand then yield clName()
            let nestedResult = fields.[0] :?> IParseResult
            yield! mkCommandLineArgs nested (nestedResult.GetAllResults())
        }

    args
    |> Seq.map (fun o -> let tag = argInfo.TagReader.Value o in argInfo.Cases.Value.[tag], o)
    |> Seq.sortBy (fun (aI,_) -> aI.CliPosition.Value)
    |> Seq.collect (fun (aI,o) -> mkEntry aI o)

/// <summary>
///     returns an App.Config XDocument given a set of config parameters
/// </summary>
let mkAppSettingsDocument (argInfo : UnionArgInfo) printComments (args : 'Template list) =
    let mkArgumentEntry (t : 'Template) : XNode [] =
        let tag = argInfo.TagReader.Value (t :> _)
        let aI = argInfo.Cases.Value.[tag]
        let getFields () = aI.FieldReader.Value (t :> _)

        let mkElem (mkComment : unit -> string) (key : string) (value : string) : XNode [] =
            let xelem =
                XElement(XName.Get "add",
                    XAttribute(XName.Get "key", key),
                    XAttribute(XName.Get "value", value))

            if printComments then
                [|XComment (mkComment()); xelem|]
            else
                [|xelem|]

        match aI.AppSettingsName.Value with
        | None -> [||]
        | Some key ->
            match aI.ParameterInfo.Value with
            | SubCommand _ -> [||]
            | Primitives parsers ->
                let values =
                    match getFields() with
                    | [||] -> "true"
                    | fields ->
                        (fields, parsers)
                        ||> Seq.map2 (fun f p -> p.UnParser f)
                        |> String.concat aI.AppSettingsSeparators.[0]

                let mkComment () =
                    stringExpr {
                        yield ' '
                        yield aI.Description.Value

                        match parsers |> Array.toList with
                        | [] -> ()
                        | first :: rest ->
                            yield " : "
                            yield first.Description
                            for p in rest do
                                yield aI.AppSettingsSeparators.[0]
                                yield p.Description

                        yield ' '

                    } |> StringExpr.build

                mkElem mkComment key values

            | ListParam(ex,fp) ->
                ex.Accept { new IFunc<XNode []> with
                    member __.Invoke<'T> () =
                        let values =
                            getFields().[0] :?> 'T list
                            |> Seq.map (fun t -> fp.UnParser (t :> _))
                            |> String.concat aI.AppSettingsSeparators.[0]

                        let mkComment () = sprintf " %s : %s ..." aI.Description.Value fp.Description

                        mkElem mkComment key values }

            | OptionalParam(ex,fp) ->
                ex.Accept { new IFunc<XNode []> with
                    member __.Invoke<'T> () =
                        let value =
                            match getFields().[0] :?> 'T option with
                            | None -> ""
                            | Some t -> fp.UnParser (t :> _)

                        let mkComment () = sprintf " %s : ?%s" aI.Description.Value fp.Description

                        mkElem mkComment key value }

    XDocument(
        XElement(XName.Get "configuration",
            XElement(XName.Get "appSettings", Seq.collect mkArgumentEntry args)))