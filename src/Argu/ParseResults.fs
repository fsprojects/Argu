namespace Argu

open FSharp.Quotations

type internal IParseResults =
    abstract GetAllResults : unit -> obj list
    

/// Argument parsing result holder.
type ParseResults<'Template when 'Template :> IArgParserTemplate> 
    internal (argInfo : UnionArgInfo, results : UnionParseResults, 
                mkUsageString : string option -> string, exiter : IExiter) =

    // exiter wrapper
    let exit hideUsage msg id =
        if hideUsage then exiter.Exit(msg, id)
        else exiter.Exit(mkUsageString (Some msg), id)

    // restriction predicate based on optional parse source
    let restrictF flags : UnionCaseParseResult -> bool =
        let flags = defaultArg flags ParseSource.All
        fun x -> Enum.hasFlag flags x.Source

    let getResults rs (e : Expr) = results.Cases.[expr2ArgId(e).Tag] |> Seq.filter (restrictF rs)
    let containsResult rs (e : Expr) = e |> getResults rs |> Seq.isEmpty |> not
    let tryGetResult rs (e : Expr) = e |> getResults rs |> Seq.tryLast
    let getResult rs (e : Expr) =
        let id = expr2ArgId e
        let results = results.Cases.[id.Tag]
        match Seq.tryLast results with
        | None -> 
            let aI = argInfo.Cases.[id.Tag]
            exit aI.NoCommandLine (sprintf "missing argument '%s'." aI.Name) (int ErrorCode.PostProcess)
        | Some r -> 
            if restrictF rs r then r
            else
                let aI = r.ArgInfo
                exit aI.NoCommandLine (sprintf "missing argument '%s'." aI.Name) (int ErrorCode.PostProcess)

    let parseResult (f : 'F -> 'S) (r : UnionCaseParseResult) =
        try f (r.FieldContents :?> 'F)
        with e ->
            exit r.ArgInfo.NoCommandLine (sprintf "Error parsing '%s': %s" r.ParseContext e.Message) (int ErrorCode.PostProcess)

    interface IParseResults with    
        member __.GetAllResults () =
            __.GetAllResults() |> List.map box

    /// Returns true if '--help' parameter has been specified in the command line.
    member __.IsUsageRequested = results.IsUsageRequested
        
    /// <summary>Returns the usage string.</summary>
    /// <param name="message">The message to be displayed on top of the usage string.</param>
    member __.Usage(?message : string) = mkUsageString message

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