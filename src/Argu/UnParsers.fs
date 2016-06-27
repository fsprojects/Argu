[<AutoOpen>]
module internal Argu.UnParsers

open System
open System.Text
open System.Xml
open System.Xml.Linq

open FSharp.Reflection

/// <summary>
///     print the command line syntax
/// </summary>
let printCommandLineSyntax (argInfo : UnionArgInfo) (programName : string) = stringExpr {
    yield programName

    for command in getHierarchy argInfo do
        yield ' '
        yield command.Name
    
    let sorted = 
        argInfo.Cases
        |> Seq.filter (fun aI -> not aI.IsHidden)
        |> Seq.sortBy (fun aI -> not aI.IsFirst, aI.IsRest || aI.IsNested, aI.Tag)
        |> Seq.toArray

    for aI in sorted do
        match aI.CommandLineNames with
        | [] -> ()
        | name :: _ ->
            yield ' '
            if not aI.IsMandatory then yield '['
            yield name

            match aI.FieldParsers with
            | Primitives parsers ->
                if aI.IsEquals1Assignment then
                    assert(parsers.Length = 1)
                    yield sprintf "=<%s>" parsers.[0].Description
                elif aI.IsEquals2Assignment then
                    assert(parsers.Length = 2)
                    yield sprintf " <%s>=<%s>" parsers.[0].Description parsers.[1].Description
                else
                    for p in parsers do
                        yield sprintf " <%s>" p.Description

                if aI.IsRest then yield " ..."

            | OptionalParam (_,parser) ->
                if aI.IsEquals1Assignment then
                    yield sprintf "[=<%s>]" parser.Description
                else
                    yield sprintf " [<%s>]" parser.Description

            | NestedUnion _ -> yield " <subcommands>"
            | ListParam (_,parser) ->
                yield sprintf " <%s> ..." parser.Description

            if not aI.IsMandatory then yield ']'

    match argInfo.HelpParam.Flags with
    | h :: _ -> yield sprintf " [%s]" h
    | _ -> ()
}
 
/// <summary>
///     print usage string for given arg info
/// </summary>
let printArgUsage (aI : UnionCaseArgInfo) = stringExpr {
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

        match aI.FieldParsers with
        | Primitives fieldParsers ->
            if aI.IsEquals1Assignment then
                assert (fieldParsers.Length = 1)
                yield sprintf "=<%s>" fieldParsers.[0].Description
            elif aI.IsEquals2Assignment then
                assert (fieldParsers.Length = 2)
                yield sprintf " <%s>=<%s>" fieldParsers.[0].Description fieldParsers.[1].Description
            else
                for p in fieldParsers do
                    yield sprintf " <%s>" p.Description

            if aI.IsRest then yield " ..."

        | OptionalParam (_,parser) ->
            if aI.IsEquals1Assignment then
                yield sprintf "?=<%s>" parser.Description
            else
                yield sprintf " <?%s>" parser.Description

        | ListParam (_,parser) ->
            yield sprintf " <%s ...>" parser.Description

        | NestedUnion _ ->
            yield " <options>"

        yield ": "
        yield aI.Usage
        yield Environment.NewLine
}

/// <summary>
///     print usage string for given help param
/// </summary>
let printHelpParam (hp : HelpParam) = stringExpr {
    match hp.Flags with
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

        yield ": "
        yield hp.Description
        yield Environment.NewLine
}

/// <summary>
///     print usage string for a collection of arg infos
/// </summary>
let printUsage (argInfo : UnionArgInfo) programName (description : string option) (msg : string option) = stringExpr {
    match msg with
    | Some u -> yield u
    | None -> 
        match description with
        | Some d -> yield d ; yield Environment.NewLine; yield Environment.NewLine
        | None -> ()

        yield "USAGE: " ; yield! printCommandLineSyntax argInfo programName

    let options, subcommands =
        argInfo.Cases
        |> Seq.filter (fun aI -> not aI.IsHidden)
        |> Seq.partition (fun aI -> not aI.IsNested)

    if options.Length > 0 || argInfo.UsesHelpParam then
        yield Environment.NewLine; yield Environment.NewLine
        yield "OPTIONS:"
        yield Environment.NewLine; yield Environment.NewLine

        for aI in options do yield! printArgUsage aI

        yield! printHelpParam argInfo.HelpParam

    if subcommands.Length > 0 then
        yield Environment.NewLine
        yield "SUBCOMMANDS:"
        yield Environment.NewLine; yield Environment.NewLine

        for aI in subcommands do yield! printArgUsage aI

        match argInfo.HelpParam.Flags with
        | [] -> ()
        | helpflag :: _ -> 
            yield Environment.NewLine
            yield sprintf "\tUse '%s <subcommand> %s' for additional information." programName helpflag
            yield Environment.NewLine
}

/// <summary>
///     print a command line argument for a set of parameters   
/// </summary>
/// <param name="argInfo"></param>
/// <param name="args"></param>
let rec printCommandLineArgs (argInfo : UnionArgInfo) (args : seq<obj>) =
    let printEntry (t : obj) = seq {
        let tag = argInfo.TagReader.Value t
        let aI = argInfo.Cases.[tag]
        let fields = aI.FieldReader.Value t
        
        match aI.CommandLineNames with
        | [] -> ()
        | clname :: _ ->
            match aI.FieldParsers with
            | Primitives parsers ->  
                let inline unpars i = parsers.[i].UnParser fields.[i]
                if aI.IsEquals1Assignment then
                    yield sprintf "%s=%s" clname (unpars 0)

                elif aI.IsEquals2Assignment then
                    yield clname
                    yield sprintf "%s=%s" (unpars 0) (unpars 1)
                else
                    yield clname

                    for i = 0 to fields.Length - 1 do
                        yield unpars i

            | OptionalParam(existential, parser) ->
                let optional = 
                    existential.Accept { new IFunc<obj option> with
                        member __.Invoke<'T> () = fields.[0] :?> 'T option |> Option.map box }

                match optional with
                | None -> yield clname
                | Some v when aI.IsEquals1Assignment -> yield sprintf "%s=%s" clname (parser.UnParser v)
                | Some v -> yield clname ; yield parser.UnParser v

            | ListParam(_, parser) ->
                yield clname
                let objSeq = fields.[0] |> unbox<System.Collections.IEnumerable> |> Seq.cast<obj>
                for obj in objSeq do yield parser.UnParser obj

            | NestedUnion (_, nested) ->
                yield clname
                let nestedResult = fields.[0] :?> IParseResults
                yield! printCommandLineArgs nested (nestedResult.GetAllResults())
        }

    args |> Seq.collect printEntry

/// <summary>
///     returns an App.Config XElement given a set of config parameters
/// </summary>
/// <param name="argInfo"></param>
/// <param name="printComments"></param>
/// <param name="args"></param>
let printAppSettings (argInfo : UnionArgInfo) printComments (args : 'Template list) =
    let printEntry (t : 'Template) : XNode [] =
        let tag = argInfo.TagReader.Value (t :> _)
        let aI = argInfo.Cases.[tag]
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

        match aI.AppSettingsName with
        | None -> [||]
        | Some key ->
            match aI.FieldParsers with
            | NestedUnion _ -> [||]
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
                        yield aI.Usage

                        match parsers |> Array.toList with
                        | [] -> ()
                        | first :: rest ->
                            yield " : "
                            yield first.Description
                            for p in rest do
                                yield aI.AppSettingsSeparators.[0]
                                yield p.Description

                        yield ' '

                    } |> String.build

                mkElem mkComment key values

            | ListParam(ex,fp) ->
                ex.Accept { new IFunc<XNode []> with
                    member __.Invoke<'T> () =
                        let values =
                            getFields().[0] :?> 'T list
                            |> Seq.map (fun t -> fp.UnParser (t :> _))
                            |> String.concat aI.AppSettingsSeparators.[0]

                        let mkComment () = sprintf " %s : %s ..." aI.Usage fp.Description

                        mkElem mkComment key values }

            | OptionalParam(ex,fp) ->
                ex.Accept { new IFunc<XNode []> with
                    member __.Invoke<'T> () =
                        let value =
                            match getFields().[0] :?> 'T option with
                            | None -> ""
                            | Some t -> fp.UnParser (t :> _)

                        let mkComment () = sprintf " %s : ?%s" aI.Usage fp.Description

                        mkElem mkComment key value }

    XDocument(
        XElement(XName.Get "configuration",
            XElement(XName.Get "appSettings", Seq.collect printEntry args)))