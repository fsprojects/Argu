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
    member internal _.ProgramName = programName
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
    member _.GetResults ([<ReflectedDefinition>] expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : 'Fields list =
        expr |> getResults source |> Seq.map (fun r -> r.FieldContents :?> 'Fields) |> Seq.toList

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
    member _.TryGetResult ([<ReflectedDefinition>] expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : 'Fields option =
        expr |> tryGetResult source |> Option.map (fun r -> r.FieldContents :?> 'Fields)

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
    member s.GetResult ([<ReflectedDefinition>] expr : Expr<'Fields -> 'Template>, ?defaultValue : 'Fields, ?source : ParseSource) : 'Fields =
        match defaultValue with
        | None -> let r = getResult source expr in r.FieldContents :?> 'Fields
        | Some def -> defaultArg (s.TryGetResult(expr, ?source = source)) def

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="defThunk">Function used to default if no parameter has been specified.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member s.GetResult ([<ReflectedDefinition>] expr : Expr<'Fields -> 'Template>, defThunk : unit -> 'Fields, ?source : ParseSource) : 'Fields =
        s.TryGetResult expr)
        |> Option.defaultWith defThunk

    /// <summary>Checks if parameter of specific kind has been specified.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.Contains ([<ReflectedDefinition>] expr : Expr<'Template>, ?source : ParseSource) : bool = containsResult source expr
    /// <summary>Checks if parameter of specific kind has been specified.</summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member _.Contains ([<ReflectedDefinition>] expr : Expr<'Fields -> 'Template>, ?source : ParseSource) : bool = containsResult source expr

    /// <summary>Raise an error through the argument parser's exiter mechanism. Display usage optionally.</summary>
    /// <param name="msg">The error message to be displayed.</param>
    /// <param name="errorCode">The error code to be returned.</param>
    /// <param name="showUsage">Print usage together with error message.</param>
    member _.Raise (msg : string, ?errorCode : ErrorCode, ?showUsage : bool) : 'T =
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
    member r.PostProcessResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R =
        expr |> getResult source |> parseResult parser

    /// <summary>Query parse results for given argument kind.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member r.PostProcessResults ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R list =
        expr |> getResults source |> Seq.map (parseResult parser) |> Seq.toList

    /// <summary>Returns the *last* specified parameter of given type.
    ///          Command line parameters have precedence over AppSettings parameters.
    ///          Results are passed to a post-processing function that is error handled by the parser.
    /// </summary>
    /// <param name="expr">The name of the parameter, expressed as quotation of DU constructor.</param>
    /// <param name="parser">The post-processing parser.</param>
    /// <param name="source">Optional source restriction: AppSettings or CommandLine.</param>
    member r.TryPostProcessResult ([<ReflectedDefinition>] expr : Expr<'Field -> 'Template>, parser : 'Field -> 'R, ?source) : 'R option =
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
