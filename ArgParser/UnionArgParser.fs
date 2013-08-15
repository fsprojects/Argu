namespace UnionArgParser

    open System
    open System.Configuration
    open System.Reflection
    open System.Xml
    open System.Xml.Linq

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    open UnionArgParser.Utils
    open UnionArgParser.ArgInfo
    open UnionArgParser.Parsers
    open UnionArgParser.Printers

    /// The UnionArgParser type generates an argument parser given a type argument
    /// that is an F# discriminated union. It can then be used to parse command line arguments
    /// or XML configuration.
    type UnionArgParser<'Template when 'Template :> IArgParserTemplate> (?usageText : string) =
        do 
            if not <| FSharpType.IsUnion typeof<'Template> then
                invalidArg typeof<'Template>.Name "UnionArgParser: template type must be F# DU."

        let argInfo =
            typeof<'Template>
            |> FSharpType.GetUnionCases
            |> Seq.map preComputeArgInfo
            |> Seq.sortBy (fun a -> a.UCI.Tag)
            |> Seq.toList

        let clArgIdx =
            argInfo
            |> Seq.map (fun aI -> aI.CommandLineNames |> Seq.map (fun name -> name, aI))
            |> Seq.concat
            |> Map.ofSeq

        let (|ParserExn|_|) (e : exn) =
            match e with
            // do not display usage for App.Config-only parameter errors
            | Bad (msg, id, Some aI) when aI.NoCommandLine -> Some(id, msg)
            | Bad (msg, id, _) -> Some (id, printUsage (Some msg) argInfo)
            | HelpText -> Some (ErrorCode.HelpText, printUsage usageText argInfo)
            | _ -> None

        /// <summary>Parse command line arguments only.</summary>
        /// <param name="inputs">The command line input. Taken from System.Environment if not specified.</param>
        /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
        /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute.</param>
        member s.ParseCommandLine (?inputs : string [], ?errorHandler: IExiter, ?ignoreMissing) =
            let ignoreMissing = defaultArg ignoreMissing false
            let errorHandler = defaultArg errorHandler <| ExceptionExiter.ArgumentExceptionExiter()
            let inputs = match inputs with None -> getEnvArgs () | Some args -> args

            try
                let commandLineResults = parseCommandLine clArgIdx inputs
                let results = combine argInfo ignoreMissing None (Some commandLineResults)

                ArgParseResults<_>(s, results, errorHandler)
            with
            | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

        /// <summary>Parse AppSettings section of XML configuration only.</summary>
        /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
        /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute.</param>
        member s.ParseAppSettings (?errorHandler: IExiter, ?ignoreMissing) =
            let ignoreMissing = defaultArg ignoreMissing false
            let errorHandler = defaultArg errorHandler <| ExceptionExiter.ArgumentExceptionExiter()
            
            try
                let appSettingsResults = parseAppSettings argInfo
                let results = combine argInfo ignoreMissing (Some appSettingsResults) None

                ArgParseResults<_>(s, results, errorHandler)
            with
            | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

        /// <summary>Parse both command line args and AppSettings section of XML configuration.
        ///          Results are merged with command line args overriding XML config.</summary>
        /// <param name="inputs">The command line input. Taken from System.Environment if not specified.</param>
        /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
        /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute.</param>
        member s.Parse (?inputs : string [], ?errorHandler : IExiter, ?ignoreMissing) =
            let ignoreMissing = defaultArg ignoreMissing false
            let errorHandler = defaultArg errorHandler <| ExceptionExiter.ArgumentExceptionExiter()
            let inputs = match inputs with None -> getEnvArgs () | Some args -> args

            try
                let appSettingsResults = parseAppSettings argInfo
                let commandLineResults = parseCommandLine clArgIdx inputs
                let results = combine argInfo ignoreMissing (Some appSettingsResults) (Some commandLineResults)

                ArgParseResults<_>(s, results, errorHandler)
            with
            | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

        /// <summary>Returns the usage string.</summary>
        member __.Usage (?msg : string) : string = printUsage msg argInfo

        /// <summary>Prints parameters in command line format. Useful for argument string generation.</summary>
        member __.PrintCommandLine (args : 'Template list) : string [] =
            printCommandLineArgs argInfo args

        /// <summary>Prints parameters in App.Config format.</summary>
        member __.PrintAppSettings (args : 'Template list, ?printComments) : string =
            let printComments = defaultArg printComments true
            let xelem = printAppSettings argInfo printComments args
            use writer = new System.IO.StringWriter()
            xelem.Save writer
            writer.Flush()
            writer.ToString()
            
    /// Argument parsing result holder
    and ArgParseResults<'Template when 'Template :> IArgParserTemplate> 
            internal (ap : UnionArgParser<'Template>, results : Map<ArgId, ArgInfo * ParseResult<'Template> list>, exiter : IExiter) =

        // exiter wrapper
        let exit hideUsage msg id =
            if hideUsage then exiter.Exit(msg, id)
            else exiter.Exit(ap.Usage msg, id)

        // restriction predicate based on optional parse source
        let restrictF src : ParseResult<'T> -> bool = 
            match src with
            | None -> fun _ -> true
            | Some src -> fun x -> x.Source = src

        let getResults rs (e : Expr) = results.[expr2ArgId e] |> snd |> List.filter (restrictF rs)
        let containsResult rs (e : Expr) = e |> getResults rs |> List.isEmpty |> not
        let tryGetResult rs (e : Expr) = e |> getResults rs |> List.tryLast
        let getResult rs (e : Expr) =
            let id = expr2ArgId e
            let aI, results = results.[id]
            match List.tryLast results with
            | None -> exit aI.NoCommandLine (sprintf "missing argument '%s'." <| getName aI) (int ErrorCode.PostProcess)
            | Some r -> 
                if restrictF rs r then r
                else
                    exit aI.NoCommandLine (sprintf "missing argument '%s'." <| getName aI) (int ErrorCode.PostProcess)

        let parseResult (f : 'F -> 'S) (r : ParseResult<'T>) =
            try f (r.FieldContents :?> 'F)
            with e ->
                exit r.ArgInfo.NoCommandLine (sprintf "Error parsing '%s': %s" r.ParseContext e.Message) (int ErrorCode.PostProcess)

        /// <summary>Query parse results for parameterless argument.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.GetResults (expr : Expr<'Template>, ?source : ParseSource) : 'Template list = 
            expr |> getResults source |> List.map (fun r -> r.Value)

        /// <summary>Query parse results for argument with parameters.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.GetResults (expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : 'Fields list = 
            expr |> getResults source |> List.map (fun r -> r.FieldContents :?> 'Fields)

        /// <summary>Gets all parse results.</summary>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.GetAllResults (?source : ParseSource) : 'Template list = 
            results 
            |> Seq.collect (fun (KeyValue(_,(_,rs))) -> rs)
            |> Seq.filter (restrictF source)
            |> Seq.map (fun r -> r.Value)
            |> Seq.toList

        /// <summary>Returns the *last* specified parameter of given type, if it exists. 
        ///          Command line parameters have precedence over AppSettings parameters.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.TryGetResult (expr : Expr<'Template>, ?source : ParseSource) : 'Template option = 
            expr |> tryGetResult source |> Option.map (fun r -> r.Value)

        /// <summary>Returns the *last* specified parameter of given type, if it exists. 
        ///          Command line parameters have precedence over AppSettings parameters.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.TryGetResult (expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : 'Fields option = 
            expr |> tryGetResult source |> Option.map (fun r -> r.FieldContents :?> 'Fields)

        /// <summary>Returns the *last* specified parameter of given type. 
        ///          Command line parameters have precedence over AppSettings parameters.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="defaultValue">Return this of no parameter of specific kind has been specified.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member s.GetResult (expr : Expr<'Template>, ?defaultValue : 'Template, ?source : ParseSource) : 'Template =
            match defaultValue with
            | None -> let r = getResult source expr in r.Value
            | Some def -> defaultArg (s.TryGetResult(expr, ?source = source)) def
                
        /// <summary>Returns the *last* specified parameter of given type. 
        ///          Command line parameters have precedence over AppSettings parameters.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="defaultValue">Return this of no parameter of specific kind has been specified.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member s.GetResult (expr : Expr<'Fields -> 'Template>, ?defaultValue : 'Fields , ?source : ParseSource) : 'Fields =
            match defaultValue with
            | None -> let r = getResult source expr in r.FieldContents :?> 'Fields
            | Some def -> defaultArg (s.TryGetResult expr) def

        /// <summary>Checks if parameter of specific kind has been specified.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.Contains (expr : Expr<'Template>, ?source : ParseSource) : bool = containsResult source expr
        /// <summary>Checks if parameter of specific kind has been specified.</summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member __.Contains (expr : Expr<_ -> 'Template>, ?source : ParseSource) : bool = containsResult source expr

        /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
        /// <param name="msg">The error message to be displayed.</param>
        /// <param name="errorCode">The error code to returned.</param>
        /// <param name="showUsage">Print usage together with error message.</param>
        member __.Raise (msg, ?errorCode : int, ?showUsage : bool) : 'T =
            let showUsage = defaultArg showUsage true
            exit (not showUsage) msg (defaultArg errorCode (int ErrorCode.PostProcess))

        /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
        /// <param name="e">The error to be displayed.</param>
        /// <param name="errorCode">The error code to returned.</param>
        /// <param name="showUsage">Print usage together with error message.</param>
        member r.Raise (e : exn, ?errorCode : int, ?showUsage : bool) : 'T = 
            r.Raise (e.Message, ?errorCode = errorCode, ?showUsage = showUsage)

        /// <summary>Handles any raised exception through the argument parser's exiter mechanism. Display usage optionally.</summary>
        /// <param name="f">The operation to be executed.</param>
        /// <param name="errorCode">The error code to returned.</param>
        /// <param name="showUsage">Print usage together with error message.</param>
        member r.Catch (f : unit -> 'T, ?errorCode : int, ?showUsage : bool) : 'T =
            try f () with e -> r.Raise(e.Message, ?errorCode = errorCode, ?showUsage = showUsage)

        /// <summary>Returns the *last* specified parameter of given type. 
        ///          Command line parameters have precedence over AppSettings parameters.
        ///          Results are passed to a post-processing function that is error handled by the parser.
        /// </summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="parser">The post-processing parser.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member r.PostProcessResult (expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R =
            expr |> getResult source |> parseResult parser

        /// <summary>Query parse results for given argument kind.
        ///          Command line parameters have precedence over AppSettings parameters.
        ///          Results are passed to a post-processing function that is error handled by the parser.
        /// </summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="parser">The post-processing parser.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member r.PostProcessResults (expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R list =
            expr |> getResults source |> List.map (parseResult parser)

        /// <summary>Returns the *last* specified parameter of given type. 
        ///          Command line parameters have precedence over AppSettings parameters.
        ///          Results are passed to a post-processing function that is error handled by the parser.
        /// </summary>
        /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
        /// <param name="parser">The post-processing parser.</param>
        /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
        member r.TryPostProcessResult (expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R option =
            expr |> tryGetResult source |> Option.map (parseResult parser)