[<AutoOpen>]
module internal Argu.Parsers
        
open System
open System.Configuration
open System.Collections.Generic
open System.Text.RegularExpressions
open System.IO

exception HelpText
exception ParseError of string * ErrorCode * UnionCaseArgInfo option

let inline error caseInfo code fmt = Printf.ksprintf (fun msg -> raise <| ParseError(msg, code, caseInfo)) fmt

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
    let results = argInfo.Cases |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult> ())

    member val IsUsageRequested = false with get,set
    member __.ResultCount = resultCount

    member __.AppendResult(result : UnionCaseParseResult) =
        if not result.ArgInfo.IsHelpParameter then resultCount <- resultCount + 1
        results.[result.Tag].Add result

    member __.ToUnionParseResults() = 
        { Cases = results |> Array.map (fun c -> c.ToArray()) ; 
          IsUsageRequested = __.IsUsageRequested }

type CliParseState =
    {
        ArgInfo : UnionArgInfo
        IgnoreUnrecognizedArgs : bool
        Reader : CliTokenReader
        Results : CliParseResults
        Exiter : IExiter
    }    

// parses the first part of a command line parameter
// recognizes if parameter is of kind --param arg or --param=arg
let private assignRegex = new Regex(@"^([^=]*)=(.*)$", RegexOptions.Compiled)
let private parseEqualityParam (param : string) =
    let m = assignRegex.Match param
    if m.Success then
        let name = m.Groups.[1].Value
        let param = m.Groups.[2].Value
        name, Some param
    else
        param, None

/// construct a parse result from untyped collection of parsed arguments
let mkUnionCase (info : UnionCaseArgInfo) src ctx (fields : obj []) =
    {
        Value = info.CaseCtor fields
        FieldContents =
            match info.FieldCtor.Value with
            | None -> null
            | Some ctor -> ctor fields

        ArgInfo = info
        Source = src
        ParseContext = ctx
    }

/// parse the next command line argument and append to state
let rec private parseCommandLinePartial (state : CliParseState) =
    let inline parseField (info : UnionCaseArgInfo) (name : string) (field : FieldParserInfo) (arg : string) =
        try field.Parser arg
        with _ -> 
            error (Some info) ErrorCode.CommandLine "option '%s' expects argument <%O> but was '%s'." name field arg

    if not <| state.Reader.MoveNext() then () else
    let token = state.Reader.Current

    if state.ArgInfo.UseDefaultHelper && defaultHelpParams.Contains token then
        state.Results.IsUsageRequested <- true
    else
        let name, equalityParam = parseEqualityParam token

        match state.ArgInfo.CliParamIndex.Value.TryFind name with
        | None when state.IgnoreUnrecognizedArgs -> ()
        | None -> error None ErrorCode.CommandLine "unrecognized argument: '%s'." name
        | Some caseInfo when equalityParam.IsSome && not caseInfo.IsEqualsAssignment ->
            error (Some caseInfo) ErrorCode.CommandLine "invalid CLI syntax '%s=<param>'." name
        | Some caseInfo when caseInfo.IsFirst && state.Results.ResultCount > 0 ->
            error (Some caseInfo) ErrorCode.CommandLine "argument '%s' should precede all other arguments." name

        | Some caseInfo ->
            match caseInfo.FieldParsers with
            | Primitives [|field|] when caseInfo.IsEqualsAssignment ->
                match equalityParam with
                | None -> error (Some caseInfo) ErrorCode.CommandLine "argument '%s' missing an assignment." name
                | Some eqp ->
                    let argument = parseField caseInfo name field eqp
                    let result = mkUnionCase caseInfo ParseSource.CommandLine name [| argument |]
                    state.Results.AppendResult result

            | Primitives fields ->
                let parseNextField (p : FieldParserInfo) =
                    if state.Reader.MoveNext() then
                        parseField caseInfo name p state.Reader.Current
                    else
                        error (Some caseInfo) ErrorCode.CommandLine "parameter '%s' missing argument <%O>." name p
                        
                let parseSingleParameter () =
                    let fields = fields |> Array.map parseNextField
                    let result = mkUnionCase caseInfo ParseSource.CommandLine name fields
                    state.Results.AppendResult result

                if caseInfo.IsRest then
                    while not state.Reader.IsCompleted do
                        parseSingleParameter()
                else
                    parseSingleParameter()

            | NestedUnion (existential, nestedUnion) ->
                let nestedResults = parseCommandLineInner nestedUnion state.Exiter state.IgnoreUnrecognizedArgs state.Reader
                let result = 
                    existential.Accept { new ITemplateFunc<obj> with
                        member __.Invoke<'Template when 'Template :> IArgParserTemplate> () =
                            new ParseResults<'Template>(state.ArgInfo, nestedResults, 
                                        printUsage state.ArgInfo >> String.build, state.Exiter) :> obj }

                let result = mkUnionCase caseInfo ParseSource.CommandLine name [|result|]
                state.Results.AppendResult result
                

/// <summary>
///     Parse the entire command line
/// </summary>
/// <param name="argIdx">Dictionary of all possible CL arguments.</param>
/// <param name="ignoreUnrecognized">Ignored unrecognized parameters.</param>
/// <param name="inputs">CL inputs</param>
and private parseCommandLineInner (argInfo : UnionArgInfo) (exiter : IExiter) (ignoreUnrecognized : bool) (reader : CliTokenReader) =
    let state = 
        { ArgInfo = argInfo ; IgnoreUnrecognizedArgs = ignoreUnrecognized ; 
            Reader = reader ; Results = new CliParseResults(argInfo) ; Exiter = exiter }

    while not reader.IsCompleted do parseCommandLinePartial state
    state.Results.ToUnionParseResults()

and parseCommandLine (argInfo : UnionArgInfo) (exiter : IExiter) (ignoreUnrecognized : bool) (inputs : string []) =
    let reader = new CliTokenReader(inputs)
    parseCommandLineInner argInfo exiter ignoreUnrecognized reader


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
        GetKey : string -> string
        Results : AppSettingsParseResults
    }




/// <summary>
///     Parse single AppSettings entry
/// </summary>
/// <param name="appSettingsReader">AppSettings key-value reader function.</param>
/// <param name="state">threaded parse state.</param>
/// <param name="aI">Argument Info to parse.</param>
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
                        let results = [| mkUnionCase caseInfo ParseSource.CommandLine name [||] |]
                        success results
                else
                    error (Some caseInfo) ErrorCode.AppSettings "AppSettings entry '%s' is not <bool>." name

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

                        with _ -> error (Some caseInfo) ErrorCode.AppSettings "AppSettings entry '%s' is not <%O>." name parser
                    else
                        error (Some caseInfo) ErrorCode.AppSettings "AppSettings entry '%s' missing <%O> argument." name parser

                let parseSingleArgument() =
                    let fields = fields |> Array.map parseNext
                    mkUnionCase caseInfo ParseSource.AppSettings name fields

                let results =
                    if caseInfo.AppSettingsCSV then [| while !pos < tokens.Length do yield parseSingleArgument () |]
                    else [| parseSingleArgument () |]

                success results

        | _ -> ()

    with ParseError _ as e -> state.Results.AddException caseInfo e

/// <summary>
///     Parse a given AppSettings file.  
/// </summary>
/// <param name="appConfigFile">AppConfig file to parsed. Defaults to ConfigutionManager resolution.</param>
/// <param name="argInfo">List of all possible arguments.</param>
and parseAppSettings configReader (argInfo : UnionArgInfo) =
    let state = { GetKey = configReader ; Results = new AppSettingsParseResults(argInfo) }
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
            error (Some caseInfo) ErrorCode.PostProcess "missing parameter '%s'." caseInfo.Name
        | _ -> combined

    {
        Cases = argInfo.Cases |> Array.map combineSingle
        IsUsageRequested = commandLineResults |> Option.exists (fun r -> r.IsUsageRequested)
    }