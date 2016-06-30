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
        Value = info.CaseCtor fields
        Index = index
        FieldContents =
            match info.FieldCtor.Value with
            | None -> null
            | Some ctor -> ctor fields

        CaseInfo = info
        Source = parseSource
        ParseContext = parsecontext
    }

/// Create a ParseResult<_> instance from a set of template parameters
let mkParseResultFromValues (info : UnionArgInfo) (exiter : IExiter) (width : int)
                            (programName : string) (description : string option) 
                            (values : seq<'Template>) =

    let agg = info.Cases |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult>())
    values |> Seq.iteri (fun i value ->
        let value = value :> obj
        let tag = info.TagReader.Value value
        let case = info.Cases.[tag]
        let fields = case.FieldReader.Value value
        let result = mkUnionCase case i ParseSource.None case.Name fields
        agg.[tag].Add result)

    let results = 
        { 
            IsUsageRequested = false
            UnrecognizedCliParams = []
            Cases = agg |> Array.map (fun rs -> rs.ToArray())
        }

    new ParseResult<'Template>(info, results, programName, description, width, exiter)

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