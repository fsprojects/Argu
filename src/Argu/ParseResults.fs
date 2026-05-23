namespace Argu

open FSharp.Quotations

/// Argument parsing result holder.
[<Sealed; AutoSerializable(false); StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type ParseResults<[<EqualityConditionalOn; ComparisonConditionalOn>]'Template when 'Template :> IArgParserTemplate>
    internal (argInfo : UnionArgInfo, results : UnionParseResults, programName : string, description : string option, usageStringCharWidth : int, exiter : IExiter) =

    let mkUsageString message = mkUsageString argInfo programName true usageStringCharWidth message |> StringExpr.build

    // error handler functions
    let error hideUsage code msg =
        if hideUsage then exiter.Exit(msg, code)
        else exiter.Exit(mkUsageString (Some msg), code)

    let errorf hideusage code fmt = Printf.ksprintf (error hideusage code) fmt

    // restriction predicate based on optional parse source
    let restrictF flags : UnionCaseParseResult -> bool =
        let flags = defaultArg flags ParseSource.All
        fun x -> Enum.hasFlag flags x.Source

    let getResults rs (e : Expr) = results.Cases[expr2Uci(e).Tag] |> Seq.filter (restrictF rs)
    let containsResult rs (e : Expr) = e |> getResults rs |> Seq.isEmpty |> not
    let tryGetResult rs (e : Expr) = e |> getResults rs |> Seq.tryLast
    let getResult rs (e : Expr) =
        let id = expr2Uci e
        let results = results.Cases[id.Tag]
        match Array.tryLast results with
        | None ->
            let aI = argInfo.Cases.Value[id.Tag]
            errorf (not aI.IsCommandLineArg) ErrorCode.PostProcess "ERROR: missing argument '%s'." aI.Name.Value
        | Some r when restrictF rs r -> r
        | Some r -> errorf (not r.CaseInfo.IsCommandLineArg) ErrorCode.PostProcess "ERROR: missing argument '%s'." r.CaseInfo.Name.Value

    let parseResult (f : 'F -> 'S) (r : UnionCaseParseResult) =
        try f (r.FieldContents :?> 'F)
        with e -> errorf (not r.CaseInfo.IsCommandLineArg) ErrorCode.PostProcess "ERROR parsing '%s': %s" r.ParseContext e.Message

    let getAllResults source =
        results.Cases
        |> Seq.concat
        |> Seq.filter (restrictF source)
        |> Seq.sortBy (fun r -> ((int r.Source) <<< 16) + r.Index)
        |> Seq.map (fun r -> r.Value :?> 'Template)
        |> Seq.toList

    interface IParseResult with
        member x.GetAllResults () = x.GetAllResults() |> Seq.map box

    member _.ErrorHandler = exiter
    member _.ProgramName = programName
    member internal _.Description = description
    member internal _.ArgInfo = argInfo
    member internal _.CharacterWidth = usageStringCharWidth

    /// Returns true if '--help' parameter has been specified in the command line.
    member _.IsUsageRequested = results.IsUsageRequested

    /// Gets all unrecognized CLI parameters which
    /// accumulates if parsed with 'ignoreUnrecognized = true'
    member _.UnrecognizedCliParams = results.UnrecognizedCliParams

    /// Gets all parse results that are not part of the current parsing context
    /// This is only applicable to subcommand parsing operations
    member _.UnrecognizedCliParseResults = results.UnrecognizedCliParseResults

    /// <summary>Query parse results for parameterless argument.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.GetResults ([<ReflectedDefinition>] expr : Expr<'Template>, ?source : ParseSource) : 'Template list =
        expr |> getResults source |> Seq.map (fun r -> r.Value :?> 'Template) |> Seq.toList

    /// <summary>Query parse results for argument with parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.GetResults ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, ?source : ParseSource) : 'Field list =
        expr |> getResults source |> Seq.map (fun r -> r.FieldContents :?> 'Field) |> Seq.toList

    /// <summary>Gets all parse results.</summary>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.GetAllResults (?source : ParseSource) : 'Template list =
        getAllResults source

    /// <summary>Returns the *last* specified parameter of given type, if it exists.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.TryGetResult ([<ReflectedDefinition>] expr : Expr<'Template>, ?source : ParseSource) : 'Template option =
        expr |> tryGetResult source |> Option.map (fun r -> r.Value :?> 'Template)

    /// <summary>Returns the *last* specified parameter of given type, if it exists.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.TryGetResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, ?source : ParseSource) : 'Field option =
        expr |> tryGetResult source |> Option.map (fun r -> r.FieldContents :?> 'Field)

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="defaultValue">Return this if no parameter of specific kind has been specified.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member s.GetResult ([<ReflectedDefinition>] expr : Expr<'Template>, ?defaultValue : 'Template, ?source : ParseSource) : 'Template =
        match defaultValue with
        | None -> let r = getResult source expr in r.Value :?> 'Template
        | Some def -> defaultArg (s.TryGetResult(expr, ?source = source)) def

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="defaultValue">Return this if no parameter of specific kind has been specified.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member s.GetResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, ?defaultValue : 'Field, ?source : ParseSource) : 'Field =
        match defaultValue with
        | None -> let r = getResult source expr in r.FieldContents :?> 'Field
        | Some def -> s.TryGetResult(expr, ?source = source) |> Option.defaultValue def

    /// <summary>Returns the *last* specified parameter of given type,
    ///          trapping exceptions from the <c>defThunk</c> used if no argument has been supplied.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="defThunk">Function used to default if no parameter has been specified.
    ///     Any resulting Exception will be trapped, and the Exception's <c>.Message</c> will be used as the Failure Message as per <c>Raise</c> and <c>Catch</c>.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member r.GetResult([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, defThunk : unit -> 'Field, ?source : ParseSource, ?errorCode, ?showUsage) : 'Field  =
        match r.TryGetResult(expr, ?source = source) with
        | Some x -> x
        | None -> r.Catch(defThunk, ?errorCode = errorCode, ?showUsage = showUsage)

    /// <summary>Returns the *last* specified parameter of given type,
    ///          trapping exceptions from the <c>defThunk</c> used if no argument has been supplied.
    ///          Results are passed to a post-processing function <c>parse</c> that is error handled by the parser.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="defThunk">Function used to default if no parameter has been specified.
    ///     Any resulting Exception will be trapped, and the Exception's <c>.Message</c> will be used as the Failure Message as per <c>Raise</c> and <c>Catch</c>.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message. Defaults to <c>true</c></param>
    member r.GetResult([<ReflectedDefinition>] expr: Expr<'Field -> 'Template>, defThunk: unit -> 'Field, parser: 'Field -> 'R, ?source: ParseSource, ?errorCode, ?showUsage) : 'R  =
        match expr |> tryGetResult source |> Option.map (parseResult parser) with
        | Some x -> x
        | None -> r.Catch(defThunk >> parser, ?errorCode = errorCode, ?showUsage = showUsage)

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Results are passed to a post-processing function <c>parse</c> that is error handled by the parser.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="defaultValue">Use this if no parameter of specific kind has been specified.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member r.GetResult([<ReflectedDefinition>] expr: Expr<'Field -> 'Template>, defaultValue: 'Field, parser: 'Field -> 'R, ?source, ?errorCode : ErrorCode, ?showUsage : bool) : 'R =
        match expr |> tryGetResult source |> Option.map (parseResult parser) with
        | Some x -> x
        | None -> r.Catch((fun () -> parser defaultValue), ?errorCode = errorCode, ?showUsage = showUsage)

    /// <summary>Checks if parameter of specific kind has been specified.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.Contains ([<ReflectedDefinition>] expr : Expr<'Template>, ?source : ParseSource) : bool = containsResult source expr
    /// <summary>Checks if parameter of specific kind has been specified.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.Contains ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, ?source : ParseSource) : bool = containsResult source expr

    /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
    /// <param name="msg">The error message to be displayed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member _.Raise<'T>(msg : string, ?errorCode : ErrorCode, ?showUsage : bool) : 'T =
        let errorCode = defaultArg errorCode ErrorCode.PostProcess
        let showUsage = defaultArg showUsage true
        error (not showUsage) errorCode msg

    /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
    /// <param name="error">The error to be displayed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member r.Raise<'T>(error : exn, ?errorCode : ErrorCode, ?showUsage : bool) : 'T =
        r.Raise(error.Message, ?errorCode = errorCode, ?showUsage = showUsage)

    /// <summary>Handles any raised exception through the argument parser's exiter mechanism.</summary>
    /// <param name="f">The operation to be executed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message. Defaults to <c>true</c></param>
    member r.Catch<'T>(f : unit -> 'T, ?errorCode : ErrorCode, ?showUsage : bool) : 'T =
        try f () with e -> r.Raise(e, ?errorCode = errorCode, ?showUsage = showUsage)

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    // TODO for V7 [<System.Obsolete("Please use the revised API name instead: GetResult")>]
    member r.PostProcessResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R =
        expr |> getResult source |> parseResult parser

    /// <summary>Query parse results for given argument kind.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    // TODO for V7 [<System.Obsolete("Please use the revised API name instead: GetResults")>]
    member r.PostProcessResults ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R list =
        expr |> getResults source |> Seq.map (parseResult parser) |> Seq.toList

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    // TODO for V7 [<System.Obsolete("Please use the revised API name instead: TryGetResult")>]
    member r.TryPostProcessResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R option =
        expr |> tryGetResult source |> Option.map (parseResult parser)

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member r.GetResult([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R =
        expr |> getResult source |> parseResult parser

    /// <summary>Query parse results for given argument kind.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member r.GetResults([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R list =
        expr |> getResults source |> Seq.map (parseResult parser) |> Seq.toList

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member r.TryGetResult([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R option =
        expr |> tryGetResult source |> Option.map (parseResult parser)

    /// <summary>
    ///     Iterates through *all* parse results for a given argument kind.
    ///     Command line parameters have precedence over AppSettings parameters.
    ///     Results are passed to an iterator function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="iterator">The iterator body.</param>
    /// <param name="source">Option source restriction: AppSettings or CommandLine.</param>
    member r.IterResults ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, iterator : 'Field -> unit, ?source) : unit =
        expr |> getResults source |> Seq.iter (parseResult iterator)

    /// <summary>
    ///     Iterates through the *last* parse result for a given argument kind.
    ///     Command line parameters have precedence over AppSettings parameters.
    ///     Results are passed to an iterator function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="iterator">The iterator body.</param>
    /// <param name="source">Option source restriction: AppSettings or CommandLine.</param>
    member r.IterResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, iterator : 'Field -> unit, ?source) : unit =
        expr |> tryGetResult source |> Option.iter (parseResult iterator)

    /// <summary>
    ///     Attempts to recover the subcommand parameter from the results,
    ///     if one has been specified.
    /// </summary>
    member r.TryGetSubCommand() : 'Template option =
        results.Cases
        |> Array.concat
        |> Array.tryPick(fun c ->
            if c.CaseInfo.ArgumentType = ArgumentType.SubCommand then
                Some(c.Value :?> 'Template)
            else None)

    /// <summary>
    ///     Attempts to recover the subcommand parameter from the results,
    ///     if one has been specified.
    /// </summary>
    member r.GetSubCommand() : 'Template =
        match r.TryGetSubCommand() with
        | Some sc -> sc
        | None -> error false ErrorCode.PostProcess "no valid subcommand has been specified."

    /// <summary>
    ///     Begin a fluent query for the given parameter. Compose with
    ///     <c>.FromCli()</c>, <c>.FromAppSettings()</c>, <c>.PostProcess(parser)</c>,
    ///     then call a terminal method like <c>.Get()</c>, <c>.TryGet()</c>,
    ///     <c>.GetAll()</c>, <c>.GetOrDefault(default)</c>, or <c>.Count()</c>.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as a quotation of its DU constructor.</param>
    member r.Query<'Field> ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>) : ParseResultsQuery<'Template, 'Field> =
        ParseResultsQuery<'Template, 'Field>(r, expr, None)

    // Non-reflected internal accessors used by the fluent builder. The public
    // GetResult / TryGetResult / GetResults methods carry a
    // [<ReflectedDefinition>] attribute which auto-quotes the call site, so
    // callers that already hold an Expr value cannot pass it through.
    member internal _.GetResultByExpr<'Field>(expr : Expr<'Field -> 'Template>, ?source : ParseSource) : 'Field =
        (getResult source expr).FieldContents :?> 'Field
    member internal _.TryGetResultByExpr<'Field>(expr : Expr<'Field -> 'Template>, ?source : ParseSource) : 'Field option =
        tryGetResult source expr |> Option.map (fun r -> r.FieldContents :?> 'Field)
    member internal r.GetResultOrDefaultByExpr<'Field>(expr : Expr<'Field -> 'Template>, defaultValue : 'Field, ?source : ParseSource) : 'Field =
        r.TryGetResultByExpr(expr, ?source = source) |> Option.defaultValue defaultValue
    member internal _.GetResultsByExpr<'Field>(expr : Expr<'Field -> 'Template>, ?source : ParseSource) : 'Field list =
        getResults source expr |> Seq.map (fun r -> r.FieldContents :?> 'Field) |> Seq.toList
    member internal _.ContainsByExpr<'Field>(expr : Expr<'Field -> 'Template>, ?source : ParseSource) : bool =
        containsResult source expr
    member internal _.GetParsedResultByExpr<'Field, 'R>(expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source : ParseSource) : 'R =
        getResult source expr |> parseResult parser
    member internal _.TryGetParsedResultByExpr<'Field, 'R>(expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source : ParseSource) : 'R option =
        tryGetResult source expr |> Option.map (parseResult parser)
    member internal _.GetParsedResultsByExpr<'Field, 'R>(expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source : ParseSource) : 'R list =
        getResults source expr |> Seq.map (parseResult parser) |> Seq.toList

    override r.ToString() = sprintf "%A" (r.GetAllResults())

    // used by StructuredFormatDisplay attribute
    member private r.StructuredFormatDisplay = r.ToString()

    // used by EqualityConditionalOn attribute
    // used by ComparisonConditionalOn attribute
    member val private CachedAllResults = lazy (getAllResults None) with get

    // used by EqualityConditionalOn attribute
    override r.Equals (other : obj) =
        match other with
        | :? ParseResults<'Template> as other ->
            Unchecked.equals
                r.CachedAllResults.Value
                other.CachedAllResults.Value
        | _ -> false

    // used by EqualityConditionalOn attribute
    override r.GetHashCode () =
        Unchecked.hash r.CachedAllResults.Value

    // used by ComparisonConditionalOn attribute
    interface System.IComparable with
        member r.CompareTo other =
            match other with
            | :? ParseResults<'Template> as other ->
                Unchecked.compare
                    r.CachedAllResults.Value
                    other.CachedAllResults.Value
            | _ -> invalidArg "other" "cannot compare values of different types"

/// <summary>
///     Fluent builder for a parse-result query. Chain
///     <see cref="FromCli"/> / <see cref="FromAppSettings"/> /
///     <see cref="PostProcess"/> to narrow the query, then call a
///     terminal method to obtain the value(s).
/// </summary>
and [<Sealed; NoEquality; NoComparison>]
    ParseResultsQuery<'Template, 'Field when 'Template :> IArgParserTemplate>
        internal (results : ParseResults<'Template>, expr : Expr<'Field -> 'Template>, source : ParseSource option) =

    /// Restrict the query to command-line parse results only.
    member _.FromCli () : ParseResultsQuery<'Template, 'Field> =
        ParseResultsQuery<'Template, 'Field>(results, expr, Some ParseSource.CommandLine)

    /// Restrict the query to AppSettings parse results only.
    member _.FromAppSettings () : ParseResultsQuery<'Template, 'Field> =
        ParseResultsQuery<'Template, 'Field>(results, expr, Some ParseSource.AppSettings)

    /// Restrict the query to a specific <see cref="ParseSource"/>.
    member _.FromSource (source : ParseSource) : ParseResultsQuery<'Template, 'Field> =
        ParseResultsQuery<'Template, 'Field>(results, expr, Some source)

    /// Attach a post-processing parser; the resulting query yields <c>'R</c> values.
    member _.PostProcess (parser : 'Field -> 'R) : ParseResultsParsedQuery<'Template, 'Field, 'R> =
        ParseResultsParsedQuery<'Template, 'Field, 'R>(results, expr, source, parser)

    /// Terminal: returns the last specified value. Raises through the
    /// parser's exiter if no result is present.
    member _.Get () : 'Field =
        results.GetResultByExpr(expr, ?source = source)

    /// Terminal: returns the last specified value, falling back to
    /// <paramref name="defaultValue"/> when no result is present.
    member _.GetOrDefault (defaultValue : 'Field) : 'Field =
        results.GetResultOrDefaultByExpr(expr, defaultValue, ?source = source)

    /// Terminal: returns <c>Some</c> of the last specified value, or <c>None</c> if absent.
    member _.TryGet () : 'Field option =
        results.TryGetResultByExpr(expr, ?source = source)

    /// Terminal: returns every parsed value of this parameter, in order.
    member _.GetAll () : 'Field list =
        results.GetResultsByExpr(expr, ?source = source)

    /// Terminal: returns the number of parsed values of this parameter.
    member q.Count () : int =
        q.GetAll().Length

    /// Terminal: returns <c>true</c> when at least one parse result is present.
    member _.Exists () : bool =
        results.ContainsByExpr(expr, ?source = source)

/// <summary>
///     Stage of the fluent query after <see cref="ParseResultsQuery.PostProcess"/>
///     has been applied. Terminal methods now produce values of <c>'R</c>.
/// </summary>
and [<Sealed; NoEquality; NoComparison>]
    ParseResultsParsedQuery<'Template, 'Field, 'R when 'Template :> IArgParserTemplate>
        internal (results : ParseResults<'Template>, expr : Expr<'Field -> 'Template>,
                    source : ParseSource option, parser : 'Field -> 'R) =

    /// Restrict the query to command-line parse results only.
    member _.FromCli () : ParseResultsParsedQuery<'Template, 'Field, 'R> =
        ParseResultsParsedQuery<'Template, 'Field, 'R>(results, expr, Some ParseSource.CommandLine, parser)

    /// Restrict the query to AppSettings parse results only.
    member _.FromAppSettings () : ParseResultsParsedQuery<'Template, 'Field, 'R> =
        ParseResultsParsedQuery<'Template, 'Field, 'R>(results, expr, Some ParseSource.AppSettings, parser)

    /// Restrict the query to a specific <see cref="ParseSource"/>.
    member _.FromSource (source : ParseSource) : ParseResultsParsedQuery<'Template, 'Field, 'R> =
        ParseResultsParsedQuery<'Template, 'Field, 'R>(results, expr, Some source, parser)

    /// Terminal: returns the last specified value passed through the post-processor.
    /// Raises through the parser's exiter if no result is present.
    member _.Get () : 'R =
        results.GetParsedResultByExpr(expr, parser, ?source = source)

    /// Terminal: <c>Some (parser v)</c> for the last specified value, or <c>None</c>.
    member _.TryGet () : 'R option =
        results.TryGetParsedResultByExpr(expr, parser, ?source = source)

    /// Terminal: every parsed value passed through the post-processor, in order.
    member _.GetAll () : 'R list =
        results.GetParsedResultsByExpr(expr, parser, ?source = source)
