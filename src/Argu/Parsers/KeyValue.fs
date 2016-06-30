[<AutoOpen>]
module internal Argu.KeyValueParser

open System

type KeyValueParseResult = Choice<UnionCaseParseResult [], exn>

let private emptyResult : KeyValueParseResult = Choice1Of2 [||]

// AppSettings parse errors are threaded to the state rather than raised directly
type KeyValueParseResults (argInfo : UnionArgInfo) =
    let results = Array.init argInfo.Cases.Length (fun _ -> emptyResult)
    member __.AddResults (case : UnionCaseArgInfo) (ts : UnionCaseParseResult []) =
        results.[case.Tag] <- Choice1Of2 ts

    member __.AddException (case : UnionCaseArgInfo) exn =
        results.[case.Tag] <- Choice2Of2 exn

    member __.Results : KeyValueParseResult [] = results

type KeyValueParseState =
    {
        ArgInfo : UnionArgInfo
        Reader : IConfigurationReader
        Results : KeyValueParseResults
    }

/// <summary>
///     Parse single entry from key/value configuration
/// </summary>
let private parseKeyValuePartial (state : KeyValueParseState) (caseInfo : UnionCaseArgInfo) =
    let inline success ts = state.Results.AddResults caseInfo ts

    try
        match caseInfo.AppSettingsName with
        | Some name ->
            match (try state.Reader.GetValue name with _ -> null) with
            | null | "" -> ()
            | entry ->
                match caseInfo.ParameterInfo with
                | Primitives [||] ->
                    let ok, flag = Boolean.TryParse entry
                    if ok then
                        if flag then 
                            let results = [| mkUnionCase caseInfo caseInfo.Tag ParseSource.AppSettings name [||] |]
                            success results
                    else
                        error state.ArgInfo ErrorCode.AppSettings "AppSettings entry '%s' is not <bool>." name

                | Primitives fields ->
                    let tokens = 
                        if caseInfo.AppSettingsCSV || fields.Length > 1 then
                            entry.Split(caseInfo.AppSettingsSeparators, caseInfo.AppSettingsSplitOptions)
                        else [| entry |]

                    let pos = ref 0
                    let parseNext (parser : FieldParserInfo) =
                        if !pos < tokens.Length then
                            try 
                                let tok = tokens.[!pos]
                                incr pos
                                parser.Parser tok

                            with _ -> error state.ArgInfo ErrorCode.AppSettings "AppSettings entry '%s' is not <%s>." name parser.Description
                        else
                            error state.ArgInfo ErrorCode.AppSettings "AppSettings entry '%s' missing <%s> argument." name parser.Description

                    let parseSingleArgument() =
                        let fields = fields |> Array.map parseNext
                        mkUnionCase caseInfo caseInfo.Tag ParseSource.AppSettings name fields

                    let results =
                        if caseInfo.AppSettingsCSV then [| while !pos < tokens.Length do yield parseSingleArgument () |]
                        else [| parseSingleArgument () |]

                    success results

                | OptionalParam (existential, fp) ->
                    let parsed = existential.Accept { new IFunc<obj> with
                        member __.Invoke<'T>() =
                            try (fp.Parser entry :?> 'T) |> Some :> obj
                            with _ -> error state.ArgInfo ErrorCode.AppSettings "AppSettings entry '%s' is not <%s>." name fp.Description }

                    let case = mkUnionCase caseInfo caseInfo.Tag ParseSource.AppSettings name [|parsed|]
                    success [|case|]

                | ListParam (existential, fp) ->
                    let tokens = entry.Split(caseInfo.AppSettingsSeparators, caseInfo.AppSettingsSplitOptions)
                    let results = existential.Accept { new IFunc<obj> with
                        member __.Invoke<'T>() =
                            tokens |> Seq.map (fun t -> fp.Parser t :?> 'T) |> Seq.toList :> _ }

                    let case = mkUnionCase caseInfo caseInfo.Tag ParseSource.AppSettings name [|results|]
                    success [|case|]

                | NestedUnion _ -> () // AppSettings will not handle nested arguments

        | _ -> ()
            

    with ParseError _ as e -> state.Results.AddException caseInfo e

/// <summary>
///     Parse a given key/value configuration
/// </summary>
let parseKeyValueConfig (configReader : IConfigurationReader) (argInfo : UnionArgInfo) =
    let state = { ArgInfo = argInfo ; Reader = configReader ; Results = new KeyValueParseResults(argInfo) }
    for caseInfo in argInfo.Cases do parseKeyValuePartial state caseInfo
    state.Results.Results



/// <summary>
///     Combines two parse results, AppSettings and CLI, overriding where appropriate.
///     By default, CLI parameters override AppSettings parameters.
/// </summary>
let postProcessResults (argInfo : UnionArgInfo) (ignoreMissingMandatory : bool)
                (appSettingsResults : KeyValueParseResult [] option)
                (commandLineResults : UnionParseResults option) =

    let combineSingle (caseInfo : UnionCaseArgInfo) =
        let acr = match appSettingsResults with None -> emptyResult | Some ar -> ar.[caseInfo.Tag]
        let clr = match commandLineResults with None -> [||] | Some cl -> cl.Cases.[caseInfo.Tag]

        let combined =
            match acr, clr with
            | Choice1Of2 ts, [||] -> ts
            | Choice2Of2 e, [||] -> raise e
            | Choice2Of2 e, _ when caseInfo.GatherAllSources -> raise e
            | Choice1Of2 ts, ts' when caseInfo.GatherAllSources -> Array.append ts ts'
            | _, ts' -> ts'

        match combined with
        | [||] when caseInfo.IsMandatory && not ignoreMissingMandatory -> 
            error argInfo ErrorCode.PostProcess "missing parameter '%s'." caseInfo.Name
        | _ -> combined

    {
        Cases = argInfo.Cases |> Array.map combineSingle
        UnrecognizedCliParams = match commandLineResults with Some clr -> clr.UnrecognizedCliParams | None -> []
        IsUsageRequested = commandLineResults |> Option.exists (fun r -> r.IsUsageRequested)
    }