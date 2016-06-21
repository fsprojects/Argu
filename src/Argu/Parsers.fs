[<AutoOpen>]
module internal Argu.Parsers
        
open System
open System.Configuration
open System.Collections.Generic
open System.Text.RegularExpressions
open System.IO

exception ParseError of string * ErrorCode * argInfo:UnionArgInfo
exception HelpText of subcommand:UnionArgInfo

let inline error argInfo code fmt = 
    Printf.ksprintf (fun msg -> raise <| ParseError(msg, code, argInfo)) fmt

//
//  CLI Parser
//

type CliTokenReader(inputs : string[]) =
    let mutable position = -1

    member __.IsCompleted = position = inputs.Length - 1
    member __.Position = position
    member __.Current =
        if position < 0 then null
        else inputs.[position]

    member __.MoveNext() = 
        if position < inputs.Length - 1 then
            position <- position + 1
            true
        else
            false

type CliParseResults(argInfo : UnionArgInfo) =
    let mutable resultCount = 0
    let mutable helpCount = 0
    let unrecognized = new ResizeArray<string>()
    let results = argInfo.Cases |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult> ())

    member val IsUsageRequested = false with get,set
    member __.ResultCount = resultCount
    member __.HelpCount = helpCount

    member __.AppendResult(result : UnionCaseParseResult) =
        resultCount <- resultCount + 1
        if result.ArgInfo.IsHelpParameter then helpCount <- helpCount + 1
        results.[result.Tag].Add result

    member __.AppendUnrecognized(token:string) = unrecognized.Add token

    member __.ToUnionParseResults() = 
        { Cases = results |> Array.map (fun c -> c.ToArray()) ; 
          UnrecognizedCliParams = Seq.toList unrecognized ;
          IsUsageRequested = __.IsUsageRequested }

type CliParseState =
    {
        ProgramName : string
        Exiter : IExiter
        IgnoreUnrecognizedArgs : bool
        RaiseOnUsage : bool
        Reader : CliTokenReader
    }    

// parses the first part of a command line parameter
// recognizes if parameter is of kind --param arg or --param=arg
let private assignRegex = new Regex("""^([^=]*)=(.*)$""", RegexOptions.Compiled)
let private parseEqualityParam (param : string) =
    let m = assignRegex.Match param
    if m.Success then
        let name = m.Groups.[1].Value
        let param = m.Groups.[2].Value.Trim().Trim(''', '"')
        name, Some param
    else
        param, None

/// construct a parse result from untyped collection of parsed arguments
let mkUnionCase (info : UnionCaseArgInfo) idx src ctx (fields : obj []) =
    {
        Value = info.CaseCtor fields
        Index = idx
        FieldContents =
            match info.FieldCtor.Value with
            | None -> null
            | Some ctor -> ctor fields

        ArgInfo = info
        Source = src
        ParseContext = ctx
    }

/// parse the next command line argument and append to state
let rec private parseCommandLinePartial (state : CliParseState) (argInfo : UnionArgInfo) (results : CliParseResults) =
    let inline parseField (info : UnionCaseArgInfo) (name : string) (field : FieldParserInfo) (arg : string) =
        try field.Parser arg
        with _ -> 
            error argInfo ErrorCode.CommandLine "option '%s' expects argument <%s> but was '%s'." name field.Description arg

    if not <| state.Reader.MoveNext() then () else
    let token = state.Reader.Current

    if argInfo.UseDefaultHelper && defaultHelpParams.Contains token then
        if state.RaiseOnUsage then raise <| HelpText argInfo
        else
            results.IsUsageRequested <- true
    else
        let name, equalityParam = parseEqualityParam token

        match argInfo.CliParamIndex.Value.TryFind name with
        | None -> 
            match argInfo.GroupedSwitchExtractor.Value name with
            | [||] when state.IgnoreUnrecognizedArgs -> results.AppendUnrecognized token
            | [||] -> error argInfo ErrorCode.CommandLine "unrecognized argument: '%s'." name
            | switches ->
                for sw in switches do
                    let caseInfo = argInfo.CliParamIndex.Value.[sw]
                    let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine sw [||]
                    results.AppendResult result

        | Some caseInfo when equalityParam.IsSome && not caseInfo.IsEqualsAssignment ->
            error argInfo ErrorCode.CommandLine "invalid CLI syntax '%s=<param>'." name
        | Some caseInfo when caseInfo.IsFirst && results.ResultCount - results.HelpCount > 0 ->
            error argInfo ErrorCode.CommandLine "argument '%s' should precede all other arguments." name

        | Some caseInfo ->
            if caseInfo.IsHelpParameter then 
                if state.RaiseOnUsage then raise <| HelpText argInfo
                else results.IsUsageRequested <- true

            match caseInfo.FieldParsers with
            | Primitives [|field|] when caseInfo.IsEqualsAssignment ->
                match equalityParam with
                | None -> error argInfo ErrorCode.CommandLine "argument '%s' missing an assignment." name
                | Some eqp ->
                    let argument = parseField caseInfo name field eqp
                    let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [| argument |]
                    results.AppendResult result

            | Primitives fields ->
                let parseNextField (p : FieldParserInfo) =
                    if state.Reader.MoveNext() then
                        parseField caseInfo name p state.Reader.Current
                    else
                        error argInfo ErrorCode.CommandLine "parameter '%s' missing argument <%s>." name p.Description
                        
                let parseSingleParameter () =
                    let fields = fields |> Array.map parseNextField
                    let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name fields
                    results.AppendResult result

                if caseInfo.IsRest then
                    while not state.Reader.IsCompleted do
                        parseSingleParameter()
                else
                    parseSingleParameter()

            | NestedUnion (existential, nestedUnion) ->
                let nestedResults = parseCommandLineInner state nestedUnion
                let result = 
                    existential.Accept { new ITemplateFunc<obj> with
                        member __.Invoke<'Template when 'Template :> IArgParserTemplate> () =
                            new ParseResult<'Template>(nestedUnion, nestedResults, 
                                        printUsage nestedUnion state.ProgramName >> String.build, state.Exiter) :> obj }

                let result = mkUnionCase caseInfo results.ResultCount ParseSource.CommandLine name [|result|]
                results.AppendResult result
                

/// <summary>
///     Parse the entire command line
/// </summary>
and private parseCommandLineInner (state : CliParseState) (argInfo : UnionArgInfo) =
    let results = new CliParseResults(argInfo)
    while not state.Reader.IsCompleted do parseCommandLinePartial state argInfo results
    results.ToUnionParseResults()

and parseCommandLine (argInfo : UnionArgInfo) (programName : string) (exiter : IExiter) 
                        (raiseOnUsage : bool) (ignoreUnrecognized : bool) (inputs : string []) =
    let state = {
        Reader = new CliTokenReader(inputs)
        ProgramName = programName
        RaiseOnUsage = raiseOnUsage
        IgnoreUnrecognizedArgs = ignoreUnrecognized
        Exiter = exiter
    }

    parseCommandLineInner state argInfo


//
//  App.Config parser
//

type AppSettingsParseResult = Choice<UnionCaseParseResult [], exn>

let private emptyResult : AppSettingsParseResult = Choice1Of2 [||]

// AppSettings parse errors are threaded to the state rather than raised directly
type AppSettingsParseResults (argInfo : UnionArgInfo) =
    let results = Array.init argInfo.Cases.Length (fun _ -> emptyResult)
    member __.AddResults (case : UnionCaseArgInfo) (ts : UnionCaseParseResult []) =
        results.[case.Tag] <- Choice1Of2 ts

    member __.AddException (case : UnionCaseArgInfo) exn =
        results.[case.Tag] <- Choice2Of2 exn

    member __.Results : AppSettingsParseResult [] = results

type AppSettingsParseState =
    {
        ArgInfo : UnionArgInfo
        GetKey : string -> string
        Results : AppSettingsParseResults
    }

/// <summary>
///     Parse single AppSettings entry
/// </summary>
let rec private parseAppSettingsPartial (state : AppSettingsParseState) (caseInfo : UnionCaseArgInfo) =
    let inline success ts = state.Results.AddResults caseInfo ts

    try
        match caseInfo.AppSettingsName, caseInfo.FieldParsers with
        | Some name, Primitives fields ->
            match (try state.GetKey name with _ -> null) with
            | null | "" -> ()
            | entry when fields.Length = 0 ->
                let ok, flag = Boolean.TryParse entry
                if ok then
                    if flag then 
                        let results = [| mkUnionCase caseInfo caseInfo.Tag ParseSource.AppSettings name [||] |]
                        success results
                else
                    error state.ArgInfo ErrorCode.AppSettings "AppSettings entry '%s' is not <bool>." name

            | entry ->
                let tokens = 
                    if caseInfo.AppSettingsCSV || fields.Length > 1 then entry.Split(',')
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

        | _ -> ()

    with ParseError _ as e -> state.Results.AddException caseInfo e

/// <summary>
///     Parse a given AppSettings file.  
/// </summary>
and parseAppSettings configReader (argInfo : UnionArgInfo) =
    let state = { ArgInfo = argInfo ; GetKey = configReader ; Results = new AppSettingsParseResults(argInfo) }
    for caseInfo in argInfo.Cases do parseAppSettingsPartial state caseInfo
    state.Results.Results


let getConfigurationManagerReader (appConfigFile : string option) : string -> string =
    match appConfigFile with
    | None -> fun name -> ConfigurationManager.AppSettings.[name]
    | Some file when File.Exists file ->
        let fileMap = new ExeConfigurationFileMap()
        fileMap.ExeConfigFilename <- file
        let config = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None)

        fun name ->
            match config.AppSettings.Settings.[name] with
            | null -> null
            | entry -> entry.Value

    // file not found, return null strings for everything
    | Some _ -> fun _ -> null


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
let postProcessResults (argInfo : UnionArgInfo) ignoreMissingMandatory 
                (appSettingsResults : AppSettingsParseResult [] option)
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
        | [||] when caseInfo.Mandatory && not ignoreMissingMandatory -> 
            error argInfo ErrorCode.PostProcess "missing parameter '%s'." caseInfo.Name
        | _ -> combined

    {
        Cases = argInfo.Cases |> Array.map combineSingle
        UnrecognizedCliParams = match commandLineResults with Some clr -> clr.UnrecognizedCliParams | None -> []
        IsUsageRequested = commandLineResults |> Option.exists (fun r -> r.IsUsageRequested)
    }


/// Create a ParseResult<_> instance from a set of template parameters
let mkParseResultFromValues (info : UnionArgInfo) (exiter : IExiter) 
                            (mkUsageString : string option -> string) (values : seq<'Template>) =

    let agg = info.Cases |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult>())
    let mutable i = 0
    for value in values do
        let value = value :> obj
        let tag = info.TagReader.Value value
        let case = info.Cases.[tag]
        let fields = case.FieldReader.Value value
        let result = mkUnionCase case i ParseSource.None case.Name fields
        agg.[tag].Add result
        i <- i + 1

    let results = 
        { 
            IsUsageRequested = false
            UnrecognizedCliParams = []
            Cases = agg |> Array.map (fun rs -> rs.ToArray())
        }

    new ParseResult<'Template>(info, results, mkUsageString, exiter)