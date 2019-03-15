[<AutoOpen>]
module internal Argu.CommonParsers

type KeyValueParseResult = Choice<UnionCaseParseResult [], exn>

exception ParseError of message:string * code:ErrorCode * argInfo:UnionArgInfo
exception HelpText of subcommand:UnionArgInfo

let inline error argInfo code fmt =
    Printf.ksprintf (fun msg -> raise <| ParseError("ERROR: " + msg, code, argInfo)) fmt

/// construct a parse result from untyped collection of parsed arguments
let mkUnionCase (info : UnionCaseArgInfo) index parseSource parsecontext (fields : obj []) =
    {
        Fields = fields
        Index = index
        CaseInfo = info
        Source = parseSource
        ParseContext = parsecontext
    }

/// Create a ParseResults<_> instance from a set of template parameters
let mkParseResultFromValues (info : UnionArgInfo) (exiter : IExiter) (width : int)
                            (programName : string) (description : string option)
                            (values : seq<'Template>) =

    let agg = info.Cases.Value |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult>())
    let mutable i = 0
    for value in values do
        let value = value :> obj
        let tag = info.TagReader.Value value
        let case = info.Cases.Value.[tag]
        let fields = case.FieldReader.Value value
        let result = mkUnionCase case i ParseSource.None case.Name.Value fields
        agg.[tag].Add result
        i <- i + 1

    let results =
        {
            IsUsageRequested = false
            UnrecognizedCliParams = []
            UnrecognizedCliParseResults = []
            Cases = agg |> Array.map (fun rs -> rs.ToArray())
        }

    new ParseResults<'Template>(info, results, programName, description, width, exiter)

/// <summary>
///     Combines two parse results, AppSettings and CLI, overriding where appropriate.
///     By default, CLI parameters override AppSettings parameters.
/// </summary>
let postProcessResults (argInfo : UnionArgInfo) (ignoreMissingMandatory : bool)
                (appSettingsResults : KeyValueParseResult [] option)
                (commandLineResults : UnionParseResults option) =

    let emptyResult = Choice1Of2 [||]
    let combineSingle (caseInfo : UnionCaseArgInfo) =
        let acr = match appSettingsResults with None -> emptyResult | Some ar -> ar.[caseInfo.Tag]
        let clr = match commandLineResults with None -> [||] | Some cl -> cl.Cases.[caseInfo.Tag]

        let combined =
            match acr, clr with
            | Choice1Of2 ts, [||] -> ts
            | Choice2Of2 e, [||] -> raise e
            | Choice2Of2 e, _ when caseInfo.GatherAllSources.Value -> raise e
            | Choice1Of2 ts, ts' when caseInfo.GatherAllSources.Value -> Array.append ts ts'
            | _, ts' -> ts'

        match combined with
        | [| sub |] ->
            match sub.CaseInfo.ParameterInfo.Value with
            | SubCommand (_, unionArg, __) -> 
                let errors = 
                    unionArg.Cases.Value 
                    |> Array.choose (fun case -> 
                    if case.IsMandatory.Value && not ignoreMissingMandatory then
                        Some (error unionArg ErrorCode.PostProcess "missing parameter '%s'." case.Name.Value)
                    else None
                    )
                match errors with
                | [||] -> combined
                | x -> Array.head x
            | _ -> combined
        | [||] when caseInfo.IsMandatory.Value && not ignoreMissingMandatory ->
            error argInfo ErrorCode.PostProcess "missing parameter '%s'." caseInfo.Name.Value
        | _ -> combined

    {
        Cases = argInfo.Cases.Value |> Array.map combineSingle
        UnrecognizedCliParams = match commandLineResults with Some clr -> clr.UnrecognizedCliParams | None -> []
        UnrecognizedCliParseResults = match commandLineResults with Some clr -> clr.UnrecognizedCliParseResults | None -> []
        IsUsageRequested = commandLineResults |> Option.exists (fun r -> r.IsUsageRequested)
    }