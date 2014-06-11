namespace Nessos.UnionArgParser

    open System
    open System.Text
    open System.Xml
    open System.Xml.Linq

    open Microsoft.FSharp.Reflection

    open Nessos.UnionArgParser.Utils
    open Nessos.UnionArgParser.ArgInfo

    module internal UnParsers =

        let printField (f : obj) =
            match f with
            | null -> null
            | :? bool as b -> sprintf "%b" b
            | f -> f.ToString()

        // print usage string for given arg info
        let printArgUsage (aI : ArgInfo) =
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

                    let parserNames = aI.Parsers |> Seq.map (fun p -> p.Name)
                    match aI.FieldNames with 
                    | None -> for name in parserNames do yield sprintf " <%s>" name
                    | Some(fieldNames) ->
                        let names = Seq.zip fieldNames parserNames 
                        for fieldName, parserName in names do yield (sprintf " <%s:%s>" fieldName parserName)

                    if aI.Rest then yield sprintf " ..."

                    yield ": "
                    yield aI.Usage
                    yield "\n"
            }

        // print usage string for a collection of arg infos
        let printUsage (msg : string option) (argInfo : ArgInfo list) =
            string {
                match msg with
                | None -> ()
                | Some u -> yield u + "\n"
                
                yield '\n'

                let first, last = argInfo |> List.partition (fun aI -> aI.First)

                for aI in first do
                    if not aI.Hidden then
                        yield! printArgUsage aI

                if not <| first.IsEmpty then yield '\n'

                for aI in last do 
                    if not aI.Hidden then
                        yield! printArgUsage aI

                yield! printArgUsage helpInfo
            } |> String.build

        // print a command line argument for a set of parameters
        let printCommandLineArgs (argInfo : ArgInfo list) (args : 'Template list) =
            let printEntry (t : 'Template) =
                let uci, fields = FSharpValue.GetUnionFields(t, typeof<'Template>)
                let id = ArgId uci
                let aI = argInfo |> List.find (fun aI -> id = aI.Id)

                seq {
                    match aI.CommandLineNames with
                    | [] -> ()
                    | clname :: _ ->
                        yield clname

                        for f in fields do
                            yield printField f
                }

            args |> Seq.collect printEntry |> Seq.toArray

        // returns an App.Config XElement given a set of config parameters
        let printAppSettings (argInfo : ArgInfo list) printComments (args : 'Template list) =
            let printEntry (t : 'Template) : XNode list =
                let uci, fields = FSharpValue.GetUnionFields(t, typeof<'Template>)
                let id = ArgId uci
                let aI = argInfo |> List.find (fun aI -> id = aI.Id)

                match aI.AppSettingsName with
                | None -> []
                | Some key ->
                    let values =
                        if fields.Length = 0 then "true"
                        else
                            fields
                            |> Seq.map printField
                            |> String.concat ", "

                    let xelem = 
                        XElement(XName.Get "add", 
                                    XAttribute(XName.Get "key", key), 
                                    XAttribute(XName.Get "value", values))
                    
                    if printComments then [ XComment(sprintf " %s " aI.Usage) ; xelem ]
                    else [ xelem ]

            XDocument(
                XElement(XName.Get "configuration",
                    XElement(XName.Get "appSettings", Seq.collect printEntry args)))