namespace Argu

open FSharp.Quotations

/// Argument parsing result holder.
[<Sealed; AutoSerializable(false); StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type ParseResult<'Template when 'Template :> IArgParserTemplate> 
    internal (argInfo : UnionArgInfo, results : UnionParseResults, programName : string, description : string option, usageStringCharWidth : int, exiter : IExiter) =

    let mkUsageString message = printUsage argInfo programName usageStringCharWidth message |> StringExpr.build

    // error handler functions
    let error hideUsage code msg =
        if hideUsage then exiter.Exit(msg, code)
        else exiter.Exit(mkUsageString (Some msg), code)

    let errorf hideusage code fmt = Printf.ksprintf (error hideusage code) fmt

    // restriction predicate based on optional parse source
    let restrictF flags : UnionCaseParseResult -> bool =
        let flags = defaultArg flags ParseSource.All
        fun x -> Enum.hasFlag flags x.Source

    let getResults rs (e : Expr) = results.Cases.[expr2Uci(e).Tag] |> Seq.filter (restrictF rs)
    let containsResult rs (e : Expr) = e |> getResults rs |> Seq.isEmpty |> not
    let tryGetResult rs (e : Expr) = e |> getResults rs |> Seq.tryLast
    let getResult rs (e : Expr) =
        let id = expr2Uci e
        let results = results.Cases.[id.Tag]
        match Array.tryLast results with
        | None -> 
            let aI = argInfo.Cases.[id.Tag]
            errorf aI.NoCommandLine ErrorCode.PostProcess "ERROR: missing argument '%s'." aI.Name
        | Some r when restrictF rs r -> r
        | Some r -> errorf r.ArgInfo.NoCommandLine ErrorCode.PostProcess "ERROR: missing argument '%s'." r.ArgInfo.Name

    let parseResult (f : 'F -> 'S) (r : UnionCaseParseResult) =
        try f (r.FieldContents :?> 'F)
        with e -> errorf r.ArgInfo.NoCommandLine ErrorCode.PostProcess "ERROR parsing '%s': %s" r.ParseContext e.Message

    interface IParseResult with
        member __.GetAllResults () = __.GetAllResults() |> Seq.map box

    member __.ErrorHandler = exiter
    member internal __.ProgramName = programName
    member internal __.Description = description
    member internal __.ArgInfo = argInfo
    member internal __.CharacterWidth = usageStringCharWidth

    /// Returns true if '--help' parameter has been specified in the command line.
    member __.IsUsageRequested = results.IsUsageRequested

    /// Gets all unrecognized CLI parameters which
    /// accumulates if parsed with 'ignoreUnrecognized = true'
    member __.UnrecognizedCliParams = results.UnrecognizedCliParams

    /// <summary>Query parse results for parameterless argument.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member __.GetResults (expr : Expr<'Template>, ?source : ParseSource) : 'Template list = 
        expr |> getResults source |> Seq.map (fun r -> r.Value :?> 'Template) |> Seq.toList

    /// <summary>Query parse results for argument with parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member __.GetResults (expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : 'Fields list = 
        expr |> getResults source |> Seq.map (fun r -> r.FieldContents :?> 'Fields) |> Seq.toList

    /// <summary>Gets all parse results.</summary>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member __.GetAllResults (?source : ParseSource) : 'Template list = 
        results.Cases
        |> Seq.concat
        |> Seq.filter (restrictF source)
        |> Seq.sortBy (fun r -> ((int r.Source) <<< 16) + r.Index)
        |> Seq.map (fun r -> r.Value :?> 'Template)
        |> Seq.toList

    /// <summary>Returns the *last* specified parameter of given type, if it exists. 
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member __.TryGetResult (expr : Expr<'Template>, ?source : ParseSource) : 'Template option = 
        expr |> tryGetResult source |> Option.map (fun r -> r.Value :?> 'Template)

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
        | None -> let r = getResult source expr in r.Value :?> 'Template
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
    member __.Contains (expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : bool = containsResult source expr

    /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
    /// <param name="msg">The error message to be displayed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member __.Raise (msg : string, ?errorCode : ErrorCode, ?showUsage : bool) : 'T =
        let errorCode = defaultArg errorCode ErrorCode.PostProcess
        let showUsage = defaultArg showUsage true
        error (not showUsage) errorCode msg

    /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
    /// <param name="error">The error to be displayed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member r.Raise (error : exn, ?errorCode : ErrorCode, ?showUsage : bool) : 'T = 
        r.Raise (error.Message, ?errorCode = errorCode, ?showUsage = showUsage)

    /// <summary>Handles any raised exception through the argument parser's exiter mechanism. Display usage optionally.</summary>
    /// <param name="f">The operation to be executed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member r.Catch (f : unit -> 'T, ?errorCode : ErrorCode, ?showUsage : bool) : 'T =
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
        expr |> getResults source |> Seq.map (parseResult parser) |> Seq.toList

    /// <summary>Returns the *last* specified parameter of given type. 
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member r.TryPostProcessResult (expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R option =
        expr |> tryGetResult source |> Option.map (parseResult parser)

    /// <summary>
    ///     Iterates through *all* parse results for a given argument kind.
    ///     Command line parameters have precedence over AppSettings parameters.
    ///     Results are passed to an iterator function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="iterator">The iterator body.</param>
    /// <param name="source">Option source restriction: AppSettings or CommandLine.</param>
    member r.IterResults (expr : Expr<'Field -> 'Template>, iterator : 'Field -> unit, ?source) : unit =
        expr |> getResults source |> Seq.iter (parseResult iterator)

    /// <summary>
    ///     Iterates through the *last* parse result for a given argument kind.
    ///     Command line parameters have precedence over AppSettings parameters.
    ///     Results are passed to an iterator function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="iterator">The iterator body.</param>
    /// <param name="source">Option source restriction: AppSettings or CommandLine.</param>
    member r.IterResult (expr : Expr<'Field -> 'Template>, iterator : 'Field -> unit, ?source) : unit =
        expr |> tryGetResult source |> Option.iter (parseResult iterator)

    override r.ToString() = sprintf "%A" (r.GetAllResults())

    // used by StructuredFormatDisplay attribute
    member private r.StructuredFormatDisplay = r.ToString()