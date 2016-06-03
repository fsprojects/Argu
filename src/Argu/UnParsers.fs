module internal Argu.UnParsers

open System
open System.Text
open System.Xml
open System.Xml.Linq

open Microsoft.FSharp.Reflection

open Argu.ArgInfo
 
/// <summary>
///     print usage string for given arg info
/// </summary>
/// <param name="aI"></param>
let printArgUsage (aI : ArgInfo) =
    stringExpr {
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

            if aI.IsEqualsAssignment then
                assert(aI.FieldParsers.Length = 1)
                yield sprintf "=<%O>" aI.FieldParsers.[0]
            else
                for p in aI.FieldParsers do
                    yield sprintf " <%O>" p

            if aI.IsRest then yield " ..."

            yield ": "
            yield aI.Usage
            yield "\n"
    }

/// <summary>
///     print usage string for a collection of arg infos
/// </summary>
/// <param name="msg"></param>
/// <param name="argInfo"></param>
let printUsage (msg : string option) (argInfo : ArgInfo list) =
    stringExpr {
        match msg with
        | None -> ()
        | Some u -> yield u + "\n"
                
        yield '\n'

        let first, last = argInfo |> List.partition (fun aI -> aI.IsFirst)

        for aI in first do
            if not aI.Hidden then
                yield! printArgUsage aI

        if not <| first.IsEmpty then yield '\n'

        for aI in last do 
            if not aI.Hidden then
                yield! printArgUsage aI

        yield! printArgUsage helpInfo
    } |> String.build

/// <summary>
///     print a command line argument for a set of parameters   
/// </summary>
/// <param name="argInfo"></param>
/// <param name="args"></param>
let printCommandLineArgs (argInfo : ArgInfo list) (args : 'Template list) =
    let printEntry (t : 'Template) =
        let uci, fields = FSharpValue.GetUnionFields(t, typeof<'Template>, bindingFlags = allBindings)
        let id = ArgId uci
        let aI = argInfo |> List.find (fun aI -> id = aI.Id)

        seq {
            match aI.CommandLineNames with
            | [] -> ()
            | clname :: _ when aI.IsEqualsAssignment ->
                let f = fields.[0]
                let p = aI.FieldParsers.[0]
                yield sprintf "%s='%s'" clname <| p.UnParser f

            | clname :: _ ->
                yield clname

                for f,p in Seq.zip fields aI.FieldParsers do
                    yield p.UnParser f
        }

    args |> Seq.collect printEntry |> Seq.toArray

/// <summary>
///     print the command line syntax
/// </summary>
/// <param name="argInfo"></param>
let printCommandLineSyntax (argInfo : ArgInfo list) =
    let sorted = 
        argInfo 
        |> List.filter (fun ai -> not ai.Hidden)
        |> List.sortBy (fun ai -> not ai.IsFirst, ai.IsRest)

    stringExpr {
        for aI in sorted do
            if not aI.Mandatory then yield "["
            match aI.CommandLineNames with
            | [] -> ()
            | h :: t -> 
                if aI.Mandatory && not <| List.isEmpty t then yield "("
                yield h
                for n in t do
                    yield "|"
                    yield n
                if aI.Mandatory && not <| List.isEmpty t then yield ")"
                
            match aI.IsEqualsAssignment with
            | true ->
                assert(aI.FieldParsers.Length = 1)
                yield sprintf "=<%O>" aI.FieldParsers.[0]
            | false ->
                for p in aI.FieldParsers do
                    yield sprintf " <%O>" p

            if aI.IsRest then yield " ..."

            if not aI.Mandatory then yield "]"
            if aI.Id <> (Seq.last sorted).Id then yield " "
    } |> String.build

/// <summary>
///     returns an App.Config XElement given a set of config parameters
/// </summary>
/// <param name="argInfo"></param>
/// <param name="printComments"></param>
/// <param name="args"></param>
let printAppSettings (argInfo : ArgInfo list) printComments (args : 'Template list) =
    let printEntry (t : 'Template) : XNode list =
        let uci, fields = FSharpValue.GetUnionFields(t, typeof<'Template>, bindingFlags = allBindings)
        let id = ArgId uci
        let aI = argInfo |> List.find (fun aI -> id = aI.Id)

        match aI.AppSettingsName with
        | None -> []
        | Some key ->
            let values =
                if fields.Length = 0 then "true"
                else
                    Seq.zip fields aI.FieldParsers
                    |> Seq.map (fun (f,p) -> p.UnParser f)
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

                        match aI.FieldParsers |> Array.toList with
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