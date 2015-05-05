module internal Nessos.UnionArgParser.Parsers
        
    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Configuration

    open Nessos.UnionArgParser.Utils
    open Nessos.UnionArgParser.ArgInfo

    //
    //  CLI Parser
    //

    type CliParseState<'T> =
        {
            Inputs : string []
                
            IgnoreUnrecognized : bool
            Arguments : Map<string, ArgInfo>

            Position : int
            HelpArgs : int // Help argument count
            ParseResults : Map<ArgId, ArgParseResult<'T> list>
        }
    with
        static member Init (arguments : Map<string, ArgInfo>) ignore (inputs : string []) =
            {
                Inputs = inputs
                Position = 0
                HelpArgs = 0
                IgnoreUnrecognized = ignore

                Arguments = arguments
                ParseResults = Map.empty
            } : CliParseState<'T>
            

    // parses the first part of a command line parameter
    // recognizes if parameter is of kind --param arg or --param=arg
    let private assignRegex = new Regex("^([^=]*)=(.*)$", RegexOptions.Compiled)
    let private parseEqualityParam (param : string) =
        let m = assignRegex.Match param
        if m.Success then
            let name = m.Groups.[1].Value
            let param = m.Groups.[2].Value.Trim([|''';'"'|])
            name, Some param
        else
            param, None

    /// parse the next command line argument and append to state
    let parseCommandLinePartial<'Template> (state : CliParseState<'Template>) =
        let position = ref state.Position
        let current = state.Inputs.[!position]
        do incr position

        if hasCommandLineParam helpInfo current then 
            { state with 
                HelpArgs = state.HelpArgs + 1
                Position = !position 
            }
        else
            let name, equalityParam = parseEqualityParam current

            let parseField (info : ArgInfo) (field : ParserInfo) (arg : string) =
                try field.Parser arg
                with _ ->
                    bad ErrorCode.CommandLine (Some info) 
                        "option '%s' expects argument <%O> but was '%s'." name field arg

            let updateStateWith (argInfo : ArgInfo) results =
                let previous = defaultArg (state.ParseResults.TryFind argInfo.Id) []
                { state with
                    Position = !position
                    ParseResults = state.ParseResults.Add(argInfo.Id, previous @ results)
                }

            match state.Arguments.TryFind name with
            | None when state.IgnoreUnrecognized -> { state with Position = !position }
            | None -> bad ErrorCode.CommandLine None "unrecognized argument: '%s'." name
            | Some argInfo when equalityParam.IsSome && not argInfo.IsEqualsAssignment ->
                bad ErrorCode.CommandLine (Some argInfo) "invalid CLI syntax '%s=<param>'." name
            | Some argInfo when argInfo.IsFirst && state.Position - state.HelpArgs > 0 ->
                bad ErrorCode.CommandLine (Some argInfo) "argument '%s' should precede all other arguments." name
            | Some argInfo when argInfo.IsEqualsAssignment ->
                assert (argInfo.FieldParsers.Length = 1)
                match equalityParam with
                | None -> bad ErrorCode.CommandLine (Some argInfo) "argument '%s' missing an assignment." name
                | Some eqp ->
                    let argument = parseField argInfo argInfo.FieldParsers.[0] eqp
                    let result = buildResult<'Template> argInfo ParseSource.CommandLine name [| argument |]
                    updateStateWith argInfo [ result ]

            | Some argInfo ->
                let parseNextField (p : ParserInfo) =
                    if !position < state.Inputs.Length then
                        let arg = state.Inputs.[!position]
                        incr position
                        parseField argInfo p arg
                    else
                        bad ErrorCode.CommandLine (Some argInfo) 
                            "parameter '%s' missing argument <%O>." name p
                        
                let parseSingleParameter () =
                    let fields = argInfo.FieldParsers |> Array.map parseNextField
                    buildResult<'Template> argInfo ParseSource.CommandLine name fields

                let results =
                    if argInfo.IsRest then
                        [ while !position < state.Inputs.Length do
                            yield parseSingleParameter () ]

                    else [ parseSingleParameter () ]

                updateStateWith argInfo results
                

    /// <summary>
    ///     Parse the entire command line
    /// </summary>
    /// <param name="argIdx">Dictionary of all possible CL arguments.</param>
    /// <param name="ignoreUnrecognized">Ignored unrecognized parameters.</param>
    /// <param name="inputs">CL inputs</param>
    let parseCommandLine<'Template> argIdx ignoreUnrecognized (inputs : string []) =
        let rec parsePartial (state : CliParseState<'Template>) =
            if state.Position < state.Inputs.Length then
                let state' = parseCommandLinePartial state
                parsePartial state'
            else
                state

        let init = CliParseState<'Template>.Init argIdx ignoreUnrecognized inputs
        parsePartial init


    //
    //  App.Config parser
    //

    // AppSettings parse errors are threaded to the state rather than raised directly
    type AppConfigParseState<'Template> = Map<ArgId, Choice<ArgParseResult<'Template> list, exn>>


    /// <summary>
    ///     Parse single AppSettings entry
    /// </summary>
    /// <param name="appSettingsReader">AppSettings key-value reader function.</param>
    /// <param name="state">threaded parse state.</param>
    /// <param name="aI">Argument Info to parse.</param>
    let parseAppSettingsPartial (appSettingsReader : string -> string) 
                                (state : AppConfigParseState<'Template>) (aI : ArgInfo) =

        try
            match aI.AppSettingsName with
            | None -> state
            | Some name ->
                let parseResults =
                    match appSettingsReader name with
                    | null | "" -> []
                    | entry when aI.FieldParsers.Length = 0 ->
                        match Boolean.tryParse entry with
                        | None -> bad ErrorCode.AppSettings (Some aI) "AppSettings entry '%s' is not <bool>." name
                        | Some flag when flag -> [buildResult aI ParseSource.CommandLine name [||]]
                        | Some _ -> []

                    | entry ->
                        let tokens = 
                            if aI.AppSettingsCSV || aI.FieldParsers.Length > 1 then entry.Split(',')
                            else [| entry |]

                        let pos = ref 0
                        let parseNext (parser : ParserInfo) =
                            if !pos < tokens.Length then
                                try 
                                    let tok = tokens.[!pos]
                                    incr pos
                                    parser.Parser tok
                                with _ -> 
                                    bad ErrorCode.AppSettings (Some aI) 
                                        "AppSettings entry '%s' is not <%O>." name parser
                            else
                                bad ErrorCode.AppSettings (Some aI) 
                                    "AppSettings entry '%s' missing <%O> argument." name parser
                                

                        let parseSingleArgument() =
                            let fields = aI.FieldParsers |> Array.map parseNext
                            buildResult aI ParseSource.AppSettings name fields

                        if aI.AppSettingsCSV then
                            [ while !pos < tokens.Length do
                                yield parseSingleArgument () ]

                        else [ parseSingleArgument () ]

                state.Add(aI.Id, Choice1Of2 parseResults)

        with Bad _ as e -> state.Add(aI.Id, Choice2Of2 e)

    /// <summary>
    ///     Parse a given AppSettings file.  
    /// </summary>
    /// <param name="appConfigFile">AppConfig file to parsed. Defaults to ConfigutionManager resolution.</param>
    /// <param name="argInfo">List of all possible arguments.</param>
    let parseAppSettings appConfigFile (argInfo : ArgInfo list) =
        let appSettingsReader : string -> string =
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

        List.fold (parseAppSettingsPartial appSettingsReader) Map.empty argInfo


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
    let combine (argInfo : ArgInfo list) ignoreMissing 
                    (appSettingsResults : Map<ArgId, Choice<ArgParseResult<'Template> list, exn>> option)
                    (commandLineResults : Map<ArgId, ArgParseResult<'Template> list> option) =

        let argInfo, appSettingsResults, commandLineResults =
            match appSettingsResults, commandLineResults with
            | None, None -> failwith "need at least one input source."
            | Some m, None -> List.filter isAppConfig argInfo, m, Map.empty
            | None, Some m -> List.filter isCommandLine argInfo, Map.empty, m
            | Some m, Some m' -> argInfo, m, m'

        let combineSingle (argInfo : ArgInfo) =
            let acr = defaultArg (appSettingsResults.TryFind argInfo.Id) <| Choice1Of2 []
            let clr = defaultArg (commandLineResults.TryFind argInfo.Id) []

            let combined =
                match acr, clr with
                | Choice1Of2 ts, [] -> ts
                | Choice2Of2 e, [] -> raise e
                | Choice2Of2 e, _ when argInfo.GatherAllSources -> raise e
                | Choice1Of2 ts, ts' when argInfo.GatherAllSources -> ts @ ts'
                | _, ts' -> ts'

            match combined with
            | [] when argInfo.Mandatory && not ignoreMissing -> 
                bad ErrorCode.PostProcess (Some argInfo) "missing parameter '%s'." <| getName argInfo
            | _ -> combined

        argInfo |> Seq.map (fun aI -> aI.Id, (aI, combineSingle aI)) |> Map.ofSeq