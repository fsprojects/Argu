[<AutoOpen>]
module internal Argu.Parsers
        
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.IO

exception ParseError of message:string * code:ErrorCode * argInfo:UnionArgInfo
exception HelpText of subcommand:UnionArgInfo

let inline private error argInfo code fmt =
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

        ArgInfo = info
        Source = parseSource
        ParseContext = parsecontext
    }

//
//  CLI Parser
//

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type CliParseToken =
    | EndOfStream
    | CliParam of token:string * switch:string * caseInfo:UnionCaseArgInfo * eqAssignment:string option
    | UnrecognizedOrArgument of token:string
    | GroupedParams of token:string * switches:string[] 
    | HelpArgument of token:string

type CliTokenReader(inputs : string[]) =
    let mutable position = 0
    let mutable segmentStartPos = 0

    member __.BeginCliSegment() =
        segmentStartPos <- position

    /// returns the substring that corresponds to the current argument being parsed
    /// e.g "-port 2" from "-Cf -port 2 -bar"
    member __.CurrentSegment =
        inputs.[segmentStartPos .. position - 1] |> flattenCliTokens

    member __.GetNextToken (peekOnly : bool) (argInfo : UnionArgInfo) =
        if position = inputs.Length then EndOfStream else

        let token = inputs.[position]
        if not peekOnly then position <- position + 1

        let inline extractGroupedSwitches token =
            match argInfo.GroupedSwitchExtractor.Value token with
            | [||] -> UnrecognizedOrArgument token
            | args -> GroupedParams(token, args)

        match token with
        | token when argInfo.HelpParam.IsHelpFlag token -> HelpArgument token
        | token ->
            let ok, case = argInfo.CliParamIndex.Value.TryGetValue token
            if ok then CliParam (token, token, case, None)
            else
                let mutable name = null 
                let mutable value = null
                if tryGetEqualsAssignment token &name &value then
                    let ok, case = argInfo.CliParamIndex.Value.TryGetValue name
                    if ok then CliParam(token, name, case, Some value)
                    else extractGroupedSwitches token
                else
                    extractGroupedSwitches token

    member __.MoveNext() =
        if position < inputs.Length then position <- position + 1

    member __.IsCompleted = position = inputs.Length

type CliParseResultAggregator(argInfo : UnionArgInfo) =
    let mutable resultCount = 0
    let unrecognized = new ResizeArray<string>()
    let results = argInfo.Cases |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult> ())

    member val IsUsageRequested = false with get,set
    member __.ResultCount = resultCount

    member __.AppendResult(result : UnionCaseParseResult) =
        resultCount <- resultCount + 1
        let agg = results.[result.Tag]
        if result.ArgInfo.IsUnique && agg.Count > 0 then
            error argInfo ErrorCode.CommandLine "argument '%s' has been specified more than once." result.ArgInfo.Name

        agg.Add(result)

    member __.AppendUnrecognized(token:string) = unrecognized.Add token

    member __.ToUnionParseResults() = 
        { Cases = results |> Array.map (fun c -> c.ToArray()) ; 
          UnrecognizedCliParams = Seq.toList unrecognized ;
          IsUsageRequested = __.IsUsageRequested }

type CliParseState =
    {
        ProgramName : string
        Description : string option
        UsageStringCharWidth : int
        Exiter : IExiter
        IgnoreUnrecognizedArgs : bool
        RaiseOnUsage : bool
        Reader : CliTokenReader
    }

/// parse the next command line argument and append to state
let rec private parseCommandLinePartial (state : CliParseState) (argInfo : UnionArgInfo) (results : CliParseResultAggregator) =
    state.Reader.BeginCliSegment()

    match state.Reader.GetNextToken false argInfo with
    | EndOfStream -> ()
    | HelpArgument _ when state.RaiseOnUsage -> raise <| HelpText argInfo
    | HelpArgument _ -> results.IsUsageRequested <- true
    | UnrecognizedOrArgument token when state.IgnoreUnrecognizedArgs -> results.AppendUnrecognized token
    | UnrecognizedOrArgument token -> error argInfo ErrorCode.CommandLine "unrecognized argument: '%s'." token
    | GroupedParams(_, switches) ->
        for sw in switches do
            let caseInfo = argInfo.CliParamIndex.Value.[sw]
            let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine sw [||]
            results.AppendResult result

    | CliParam(_, name, caseInfo, Some _) when not caseInfo.IsEquals1Assignment ->
        error argInfo ErrorCode.CommandLine "invalid CLI syntax '%s=<param>'." name
    | CliParam(_, name, caseInfo, _) when caseInfo.IsFirst && results.ResultCount > 0 ->
        error argInfo ErrorCode.CommandLine "argument '%s' should precede all other arguments." name

    | CliParam(token, name, caseInfo, equalityParam) ->
        match caseInfo.FieldParsers with
        | Primitives [|field|] when caseInfo.IsEquals1Assignment ->
            match equalityParam with
            | None -> error argInfo ErrorCode.CommandLine "argument '%s' missing an assignment." name
            | Some eqp ->
                let argument = 
                    try field.Parser eqp
                    with _ -> error argInfo ErrorCode.CommandLine "argument '%s' is assigned invalid value, should be <%s>." token field.Description

                let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [| argument |]
                results.AppendResult result

        | Primitives [|kf;vf|] when caseInfo.IsEquals2Assignment ->
            match state.Reader.GetNextToken true argInfo with
            | UnrecognizedOrArgument token ->
                let mutable keyTok = null
                let mutable valTok = null
                if tryGetEqualsAssignment token &keyTok &valTok then
                    let k = 
                        try kf.Parser keyTok
                        with _ -> error argInfo ErrorCode.CommandLine "argument '%s' was given invalid key '%s', should be <%s>." state.Reader.CurrentSegment token kf.Description

                    let v = 
                        try vf.Parser valTok
                        with _ -> error argInfo ErrorCode.CommandLine "argument '%s' was given invalid value assignment '%s', should be <%s>." state.Reader.CurrentSegment token vf.Description

                    let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [|k;v|]
                    results.AppendResult result
                    state.Reader.MoveNext()
                else
                    error argInfo ErrorCode.CommandLine "argument '%s' must be followed by assignment '%s=%s'" caseInfo.Name kf.Description vf.Description

            | CliParam(token,name,_,Some _) -> error argInfo ErrorCode.CommandLine "argument '%s' was given invalid key name '%s' in '%s'." state.Reader.CurrentSegment name token
            | _ -> error argInfo ErrorCode.CommandLine "argument '%s' must be followed by assignment '%s=%s'" caseInfo.Name kf.Description vf.Description

        | Primitives fields ->
            let parseNextField (p : FieldParserInfo) =
                match state.Reader.GetNextToken true argInfo with
                | UnrecognizedOrArgument token ->
                    let result =
                        try p.Parser token
                        with _ -> error argInfo ErrorCode.CommandLine "parameter '%s' must be followed by <%s>, but was '%s'." state.Reader.CurrentSegment p.Description token

                    state.Reader.MoveNext()
                    result

                | CliParam(_, name, _, _)
                | HelpArgument name 
                | GroupedParams(name,_) -> error argInfo ErrorCode.CommandLine "parameter '%s' must be followed by <%s>, but was '%s'." state.Reader.CurrentSegment p.Description name
                | _ -> error argInfo ErrorCode.CommandLine "argument '%s' must be followed by <%s>." state.Reader.CurrentSegment p.Description
                        
            let parseSingleParameter () =
                let fields = fields |> Array.map parseNextField
                let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name fields
                results.AppendResult result

            if caseInfo.IsRest then
                while not state.Reader.IsCompleted do
                    parseSingleParameter()
            else
                parseSingleParameter()

        | OptionalParam(existential, field) when caseInfo.IsEquals1Assignment ->
            let optArgument = existential.Accept { new IFunc<obj> with 
                member __.Invoke<'T> () =
                    match equalityParam with
                    | None -> Option<'T>.None :> obj
                    | Some eqp -> 
                        let argument = 
                            try field.Parser eqp
                            with _ -> error argInfo ErrorCode.CommandLine "argument '%s' is assigned invalid value, should be <%s>." token field.Description

                        argument :?> 'T |> Some :> obj }

            let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [| optArgument |]
            results.AppendResult result

        | OptionalParam(existential, field) ->
            let optArgument = existential.Accept { new IFunc<obj> with
                member __.Invoke<'T> () =
                    match state.Reader.GetNextToken true argInfo with
                    | UnrecognizedOrArgument tok -> 
                        let argument = try Some(field.Parser tok :?> 'T) with _ -> None
                        match argument with Some _ -> state.Reader.MoveNext() | None -> ()
                        argument :> obj

                    | _ -> Option<'T>.None :> obj }

            let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [| optArgument |]
            results.AppendResult result

        | ListParam(existential, field) ->
            let listArg = existential.Accept { new IFunc<obj> with
                member __.Invoke<'T>() =
                    let args = new ResizeArray<'T> ()
                    let rec gather () =
                        match state.Reader.GetNextToken true argInfo with
                        | UnrecognizedOrArgument token ->
                            let result = try Some (field.Parser token :?> 'T) with _ -> None
                            match result with
                            | None -> ()
                            | Some item ->
                                args.Add item
                                state.Reader.MoveNext()
                                gather()
                        | _ -> ()

                    do gather()
                    Seq.toList args :> obj
            }

            let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [| listArg |]
            results.AppendResult result

        | NestedUnion (existential, nestedUnion) ->
            let nestedResults = parseCommandLineInner state nestedUnion
            let result = 
                existential.Accept { new ITemplateFunc<obj> with
                    member __.Invoke<'Template when 'Template :> IArgParserTemplate> () =
                        new ParseResult<'Template>(nestedUnion, nestedResults, state.ProgramName, state.Description, state.UsageStringCharWidth, state.Exiter) :> obj }

            let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [|result|]
            results.AppendResult result

and private parseCommandLineInner (state : CliParseState) (argInfo : UnionArgInfo) =
    let results = new CliParseResultAggregator(argInfo)
    while not state.Reader.IsCompleted do parseCommandLinePartial state argInfo results
    results.ToUnionParseResults()

/// <summary>
///     Parse the entire command line
/// </summary>
and parseCommandLine (argInfo : UnionArgInfo) (programName : string) (description : string option) (width : int) (exiter : IExiter) 
                        (raiseOnUsage : bool) (ignoreUnrecognized : bool) (inputs : string []) =
    let state = {
        Reader = new CliTokenReader(inputs)
        ProgramName = programName
        Description = description
        UsageStringCharWidth = width
        RaiseOnUsage = raiseOnUsage
        IgnoreUnrecognizedArgs = ignoreUnrecognized
        Exiter = exiter
    }

    parseCommandLineInner state argInfo


//
//  App.Config parser
//

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
                match caseInfo.FieldParsers with
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

//
//  Misc parser tools
//

/// <summary>
///     Combines two parse results, AppConfig and CLI, overriding where appropriate.
///     By default, CLI parameters override AppConfig parameters.
/// </summary>
/// <param name="argInfo">List of all possible arguments.</param>
/// <param name="ignoreMissing">do not raise exception if missing mandatory parameters.</param>
/// <param name="appSettingsResults">parsed results from AppSettings</param>
/// <param name="commandLineResults">parsed results from CLI</param>
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