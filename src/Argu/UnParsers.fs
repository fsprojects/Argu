[<AutoOpen>]
module internal Argu.UnParsers

open System
open System.Text
open System.Xml
open System.Xml.Linq

open FSharp.Reflection
 
/// <summary>
///     print usage string for given arg info
/// </summary>
/// <param name="aI"></param>
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
            if aI.IsEqualsAssignment then
                assert (fieldParsers.Length = 1)
                yield sprintf "=<%O>" fieldParsers.[0]
            else
                for p in fieldParsers do
                    yield sprintf " <%O>" p

            if aI.IsRest then yield " ..."

        | NestedUnion argInfo ->
            yield "<subcommands>"

        yield ": "
        yield aI.Usage
        yield Environment.NewLine
}

let printHelpParams () = sprintf "\t%s: %s%s" defaultHelpParam defaultHelpDescription Environment.NewLine

/// <summary>
///     print usage string for a collection of arg infos
/// </summary>
/// <param name="msg"></param>
/// <param name="argInfo"></param>
let printUsage (msg : string option) (argInfo : UnionArgInfo) = stringExpr {
    match msg with
    | None -> ()
    | Some u -> yield u + Environment.NewLine
                
    yield Environment.NewLine

    for aI in argInfo.Cases do
        if not aI.Hidden then
            yield! printArgUsage aI

    if argInfo.UseDefaultHelper then
        yield printHelpParams ()
}

/// <summary>
///     print a command line argument for a set of parameters   
/// </summary>
/// <param name="argInfo"></param>
/// <param name="args"></param>
let rec printCommandLineArgs (argInfo : UnionArgInfo) (args : obj list) =
    let printEntry (t : obj) = seq {
        let tag = argInfo.TagReader.Value t
        let aI = argInfo.Cases.[tag]
        let fields = aI.FieldReader.Value t
        
        match aI.CommandLineNames with
        | [] -> ()
        | clname :: _ ->
            match aI.FieldParsers with
            | Primitives parsers ->  
                if aI.IsEqualsAssignment then
                    let f = fields.[0]
                    let p = parsers.[0]
                    yield sprintf "%s='%s'" clname <| p.UnParser f

                else
                    yield clname

                    for i = 0 to fields.Length - 1 do
                        yield parsers.[i].UnParser fields.[i]

            | NestedUnion nested ->
                yield clname
                let nestedResult = fields.[0] :?> IParseResults
                yield! printCommandLineArgs nested (nestedResult.GetAllResults())
        }

    args |> Seq.collect printEntry

/// <summary>
///     print the command line syntax
/// </summary>
/// <param name="argInfo"></param>
let rec printCommandLineSyntax (argInfo : UnionArgInfo) = stringExpr {
    let sorted = 
        argInfo.Cases
        |> Seq.filter (fun ai -> not ai.Hidden)
        |> Seq.sortBy (fun ai -> ai.IsNested, not ai.IsFirst, ai.IsRest)
        |> Seq.toArray


    for aI in sorted do
        if not aI.Mandatory then yield '['
        match aI.CommandLineNames with
        | [] -> ()
        | h :: t -> 
            if aI.Mandatory && not <| List.isEmpty t then yield '('
            yield h
            for n in t do
                yield '|'
                yield n
            if aI.Mandatory && not <| List.isEmpty t then yield ')'
                
        match aI.FieldParsers with
        | Primitives parsers ->
            if aI.IsEqualsAssignment then
                assert(parsers.Length = 1)
                yield sprintf "=<%O>" parsers.[0]
            else
                for p in parsers do
                    yield sprintf " <%O>" p

            if aI.IsRest then yield " ..."

            if not aI.Mandatory then yield ']'
            if aI.Tag <> (Seq.last sorted).Tag then yield ' '

        | NestedUnion nested -> yield! printCommandLineSyntax nested
}

/// <summary>
///     returns an App.Config XElement given a set of config parameters
/// </summary>
/// <param name="argInfo"></param>
/// <param name="printComments"></param>
/// <param name="args"></param>
let printAppSettings (argInfo : UnionArgInfo) printComments (args : 'Template list) =
    let printEntry (t : 'Template) : XNode list =
        let tag = argInfo.TagReader.Value (t :> _)
        let aI = argInfo.Cases.[tag]
        let fields = aI.FieldReader.Value (t :> _)

        match aI.AppSettingsName, aI.FieldParsers with
        | None, _ | _, NestedUnion _ -> []
        | Some key, Primitives parsers ->
            let values =
                if fields.Length = 0 then "true"
                else
                    (fields, parsers)
                    ||> Seq.map2 (fun f p -> p.UnParser f)
                    |> String.concat ", "

            let xelem = 
                XElement(XName.Get "add", 
                            XAttribute(XName.Get "key", key), 
                            XAttribute(XName.Get "value", values))
                    
            if printComments then 
                let comment =
                    stringExpr {
                        yield ' '
                        yield aI.Usage

                        match parsers |> Array.toList with
                        | [] -> ()
                        | first :: rest ->
                            yield " : "
                            yield first.ToString()
                            for p in rest do
                                yield ", "
                                yield p.ToString()

                        yield ' '

                    } |> String.build

                [ XComment(comment) ; xelem ]
            else [ xelem ]

    XDocument(
        XElement(XName.Get "configuration",
            XElement(XName.Get "appSettings", Seq.collect printEntry args)))