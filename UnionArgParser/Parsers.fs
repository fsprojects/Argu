namespace Nessos.UnionArgParser

    module internal Parsers =
        
        open System
        open System.IO
        open System.Configuration

        open Nessos.UnionArgParser.Utils
        open Nessos.UnionArgParser.ArgInfo

        // parse the next command line argument and append to state
        let parseCommandLinePartial (argIdx : Map<string, ArgInfo>) (args : string []) (pos : int ref)
                                    (isHelpRequested : bool, parseState : Map<ArgId, ParseResult<'Template> list>) =

            let curr = args.[!pos]
            do incr pos
            
            if hasCommandLineParam helpInfo curr then (true, parseState) else

            match argIdx.TryFind curr with
            | None -> bad ErrorCode.CommandLine None "unrecognized argument: '%s'." curr
            | Some argInfo when argInfo.First && !pos > 1 ->
                bad ErrorCode.CommandLine (Some argInfo) "invalid use of argument '%s'." curr
            | Some argInfo ->
                let parseOne () =
                    let fields =
                        [|
                            for p in argInfo.Parsers do
                                if !pos = args.Length then
                                    bad ErrorCode.CommandLine (Some argInfo) 
                                        "option '%s' requires argument <%s>." curr p.Name
                                yield 
                                    try p.Parser args.[!pos]
                                    with _ -> 
                                        bad ErrorCode.CommandLine (Some argInfo) 
                                            "option '%s' expects argument <%s>." curr p.Name
                                incr pos
                        |]

                    buildResult<'Template> argInfo CommandLine curr fields

                let parsedResults =
                    if argInfo.Rest then
                        [
                            while !pos < args.Length do
                                yield parseOne ()
                        ]
                    else [ parseOne () ]

                let previous = defaultArg (parseState.TryFind argInfo.Id) []
                
                isHelpRequested, parseState.Add(argInfo.Id, previous @ parsedResults)
                

        // parse the entire command line
        let parseCommandLine argIdx (inputs : string []) =
            let state = ref (false, Map.empty)
            let pos = ref 0

            while !pos < inputs.Length do
                state := parseCommandLinePartial argIdx inputs pos !state

            !state

        // AppSettings parse errors are threaded to the state rather than raised directly;
        // this happens since AppSettings errors are overriden by default in case of a valid command line input.
        let parseAppSettingsPartial (configInstance : Configuration option) 
                                    (state : Map<ArgId, Choice<ParseResult<'Template> list, exn>>) (aI : ArgInfo) =

            let getAppSettings name =
                match configInstance with
                | Some config ->
                    match config.AppSettings.Settings.[name] with
                    | null -> null
                    | entry -> entry.Value

                | None -> ConfigurationManager.AppSettings.[name]

            try
                match aI.AppSettingsName with
                | None -> state
                | Some name ->
                    let parseResults =
                        match getAppSettings name with
                        | null | "" -> []
                        | entry when aI.Parsers.Length = 0 ->
                            match Boolean.tryParse entry with
                            | None -> bad ErrorCode.AppSettings (Some aI) "AppSettings entry '%s' is not <bool>." name
                            | Some flag when flag -> [buildResult aI CommandLine name [||]]
                            | Some _ -> []
                        | entry ->
                            let tokens = 
                                if aI.AppSettingsCSV || aI.Parsers.Length > 1 then entry.Split(',') 
                                else [| entry |]

                            let pos = ref 0

                            let readNext() =
                                let fields =
                                    [|
                                        for p in aI.Parsers do
                                            if !pos < tokens.Length then
                                                yield 
                                                    try p.Parser <| tokens.[!pos]
                                                    with _ -> 
                                                        bad ErrorCode.AppSettings (Some aI) 
                                                            "AppSettings entry '%s' is not <%s>." name p.Name

                                                incr pos
                                            else
                                                bad ErrorCode.AppSettings (Some aI) 
                                                    "AppSettings entry '%s' missing <%s> argument." name p.Name
                                    |]

                                buildResult aI AppSettings name fields

                            if aI.AppSettingsCSV then
                                [
                                    while !pos < tokens.Length do
                                        yield readNext ()
                                ]
                            else [ readNext () ]

                    state.Add(aI.Id, Choice1Of2 parseResults)

            with Bad _ as e -> state.Add(aI.Id, Choice2Of2 e)

        let parseAppSettings appConfigFile (argInfo : ArgInfo list) = 
            let config =
                match appConfigFile with
                | None -> None
                | Some file ->
                    if not <| File.Exists file then 
                        raise <| new FileNotFoundException(file)

                    let fileMap = new ExeConfigurationFileMap()
                    fileMap.ExeConfigFilename <- file
                    let config = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None)
                    Some config

            List.fold (parseAppSettingsPartial config) Map.empty argInfo

        // does what the type signature says; combines two parse states into one according to the provided rules.
        let combine (argInfo : ArgInfo list) ignoreMissing 
                        (appSettingsResults : Map<ArgId, Choice<ParseResult<'Template> list, exn>> option)
                        (commandLineResults : Map<ArgId, ParseResult<'Template> list> option) =

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