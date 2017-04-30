[<AutoOpen>]
module internal Argu.CliParser

type CliParseToken =
    | EndOfStream
    | CliParam of token:string * switch:string * caseInfo:UnionCaseArgInfo * assignment:Assignment
    | UnrecognizedOrArgument of token:string
    | GroupedParams of token:string * switches:string[]
    | HelpArgument of token:string

type CliTokenReader(inputs : string[]) =
    let mutable position = 0
    let mutable segmentStartPos = 0
    let mutable isPeekedValue = false
    let mutable peekedValue = Unchecked.defaultof<CliParseToken>

    member __.BeginCliSegment() =
        segmentStartPos <- position

    /// returns the substring that corresponds to the current argument being parsed
    /// e.g "-port 2" from "-Cf -port 2 -bar"
    member __.CurrentSegment =
        inputs.[segmentStartPos .. position - 1] |> flattenCliTokens

    member __.GetNextToken (peekOnly : bool) (argInfo : UnionArgInfo) =
        // continuation which decides how to consume parsed token
        let inline kont result =
            if peekOnly then
                peekedValue <- result
                isPeekedValue <- true
            else
                position <- position + 1
                isPeekedValue <- false
                peekedValue <- Unchecked.defaultof<_>

            result

        let inline tryExtractGroupedSwitches token =
            match argInfo.GroupedSwitchExtractor.Value token with
            | [||] -> UnrecognizedOrArgument token
            | args -> GroupedParams(token, args)

        if isPeekedValue then kont peekedValue
        elif position = inputs.Length then kont EndOfStream else

        let token = inputs.[position]

        match token with
        | token when argInfo.HelpParam.IsHelpFlag token -> HelpArgument token |> kont
        | token ->
            let mutable prefix = null
            let mutable case = Unchecked.defaultof<_>
            if argInfo.CliParamIndex.Value.TryGetPrefix(token, &prefix, &case) then
                if token = prefix then CliParam(token, prefix, case, NoAssignment) |> kont
                elif case.IsCustomAssignment then
                    match case.AssignmentParser.Value token with
                    | NoAssignment -> tryExtractGroupedSwitches token
                    | assignment -> CliParam(token, prefix, case, assignment)
                    |> kont
                else
                    tryExtractGroupedSwitches token |> kont
            else
                tryExtractGroupedSwitches token |> kont

    member __.MoveNext() =
        if position < inputs.Length then
            position <- position + 1
            isPeekedValue <- false
            peekedValue <- Unchecked.defaultof<_>

    member __.IsCompleted = position = inputs.Length

type CliParseResultAggregator internal (argInfo : UnionArgInfo, stack : CliParseResultAggregatorStack) =
    let mutable resultCount = 0
    let mutable isSubCommandDefined = false
    let mutable isMainCommandDefined = false
    let mutable lastResult : UnionCaseParseResult option = None
    let unrecognized = new ResizeArray<string>()
    let unrecognizedParseResults = new ResizeArray<obj>()
    let results = lazy(argInfo.Cases.Value |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult> ()))

    member val IsUsageRequested = false with get,set

    member __.ResultCount = resultCount
    member __.IsSubCommandDefined = isSubCommandDefined
    member __.IsMainCommandDefined = isMainCommandDefined

    member __.AppendResultInner(result : UnionCaseParseResult) =
        if result.CaseInfo.IsFirst && resultCount > 0 then
            error argInfo ErrorCode.CommandLine "argument '%s' should precede all other arguments." result.ParseContext

        match lastResult with
        | Some lr when not (lr.Tag = result.CaseInfo.Tag && lr.CaseInfo.IsRest.Value) ->
            error argInfo ErrorCode.CommandLine "parameter '%s' should appear after all other arguments." lr.ParseContext
        | _ -> ()

        if result.CaseInfo.IsLast then lastResult <- Some result
        if result.CaseInfo.IsMainCommand then isMainCommandDefined <- true

        resultCount <- resultCount + 1
        let agg = results.Value.[result.Tag]
        if result.CaseInfo.IsUnique.Value && agg.Count > 0 then
            error argInfo ErrorCode.CommandLine "argument '%s' has been specified more than once." result.CaseInfo.Name.Value

        if result.CaseInfo.ArgumentType = ArgumentType.SubCommand then
            if isSubCommandDefined then
                error argInfo ErrorCode.CommandLine "cannot run multiple subcommands."
            isSubCommandDefined <- true

        agg.Add result

    member __.AppendResult caseInfo context arguments =
        let result = mkUnionCase caseInfo resultCount ParseSource.CommandLine context arguments
        if result.CaseInfo.Depth = argInfo.Depth then
            __.AppendResultInner(result)
        else
            // this parse result corresponds to an inherited parameter
            // from a parent syntax. Use the ResultAggregator stack to
            // re-route the result to its matching aggregator
            if stack.TryDispatchResult result then ()
            else unrecognizedParseResults.Add result.Value

    member __.AppendUnrecognized(token:string) = unrecognized.Add token

    member __.ToUnionParseResults() =
        { Cases = results.Value |> Array.map (fun c -> c.ToArray()) ;
          UnrecognizedCliParams = Seq.toList unrecognized ;
          UnrecognizedCliParseResults = Seq.toList unrecognizedParseResults ;
          IsUsageRequested = __.IsUsageRequested }

// this rudimentary stack implementation assumes that only one subcommand
// can occur within any particular context; no need implement popping etc.
// Note that inheritting subcommands is explicitly prohibited by the library.
and CliParseResultAggregatorStack (context : UnionArgInfo) =
    let offset = context.Depth
    let stack = new ResizeArray<CliParseResultAggregator>(capacity = 2)

    member __.TryDispatchResult(result : UnionCaseParseResult) =
        if result.CaseInfo.Depth < offset then false
        else
            stack.[result.CaseInfo.Depth - offset].AppendResultInner result
            true

    member self.CreateNextAggregator(argInfo : UnionArgInfo) =
        assert(stack.Count = argInfo.Depth - offset)
        let agg = new CliParseResultAggregator(argInfo, self)
        stack.Add agg
        agg


type CliParseState =
    {
        ProgramName : string
        Description : string option
        UsageStringCharWidth : int
        ResultStack : CliParseResultAggregatorStack
        Exiter : IExiter
        IgnoreUnrecognizedArgs : bool
        RaiseOnUsage : bool
        Reader : CliTokenReader
    }

/// parse the next command line argument and append to state
let rec private parseCommandLinePartial (state : CliParseState) (argInfo : UnionArgInfo) (aggregator : CliParseResultAggregator) =
    state.Reader.BeginCliSegment()

    match state.Reader.GetNextToken false argInfo with
    | EndOfStream -> ()
    | HelpArgument _ when state.RaiseOnUsage -> raise <| HelpText argInfo
    | HelpArgument _ -> aggregator.IsUsageRequested <- true
    | UnrecognizedOrArgument token ->
        match argInfo.MainCommandParam.Value with
        | Some mcp when not (mcp.IsUnique.Value && aggregator.IsMainCommandDefined) ->
            match mcp.ParameterInfo.Value with
            | Primitives parsers ->
                // since main command syntax deals with a degree of implicitness
                // we need a way to backtrack in case of a parse error.
                // In that case, we pass any accumulated tokens to the regular
                // unrecognized argument resolution logic
                let tokens = new ResizeArray<string>(5)
                let fields = new ResizeArray<obj>(5)
                let handleUnrecognized =
                    state.IgnoreUnrecognizedArgs ||
                    Option.isSome argInfo.UnrecognizedGatherParam.Value

                let parseSingleParameter isFirst =
                    tokens.Clear(); fields.Clear()
                    let rec aux i =
                        if i = parsers.Length then () else
                        let p = parsers.[i]
                        let isFirst = isFirst && i = 0
                        let nextToken =
                            if isFirst then UnrecognizedOrArgument token
                            else state.Reader.GetNextToken true argInfo

                        match nextToken with
                        | UnrecognizedOrArgument tok ->
                            let mutable result = null
                            let success =
                                try result <- p.Parser tok ; true
                                with
                                | _ when handleUnrecognized -> false
                                | _ when isFirst -> error argInfo ErrorCode.CommandLine "unrecognized argument: '%s'." token
                                | _ -> error argInfo ErrorCode.CommandLine "parameter '%s' must be followed by <%s>, but was '%s'."
                                                    state.Reader.CurrentSegment p.Description token

                            if success then
                                tokens.Add tok ; fields.Add result
                                if not isFirst then state.Reader.MoveNext()
                                aux (i + 1)

                        | CliParam(_, token, _, _)
                        | HelpArgument token
                        | GroupedParams(token,_) ->
                            if not handleUnrecognized then
                                error argInfo ErrorCode.CommandLine "parameter '%s' must be followed by <%s>, but was '%s'." state.Reader.CurrentSegment p.Description token
                        | _ ->
                            if not handleUnrecognized then
                                error argInfo ErrorCode.CommandLine "argument '%s' must be followed by <%s>." state.Reader.CurrentSegment p.Description

                    do aux 0
                    if fields.Count = parsers.Length then
                        aggregator.AppendResult mcp mcp.Name.Value (fields.ToArray())
                        true
                    else
                        match argInfo.UnrecognizedGatherParam.Value with
                        | Some ugp -> for tok in tokens do aggregator.AppendResult ugp token [|tok|]
                        | None when state.IgnoreUnrecognizedArgs -> for tok in tokens do aggregator.AppendUnrecognized tok
                        | None -> arguExn "internal error in main command parser."

                        false

                if parseSingleParameter true && mcp.IsRest.Value then
                    while not state.Reader.IsCompleted && parseSingleParameter false do ()

            | ListParam(existential, field) ->
                ignore <| existential.Accept { new IFunc<bool> with
                    member __.Invoke<'T>() =
                        let args = new ResizeArray<'T> ()
                        let rec gather isFirst =
                            let nextToken =
                                if isFirst then UnrecognizedOrArgument token
                                else state.Reader.GetNextToken true argInfo

                            match nextToken with
                            | UnrecognizedOrArgument token ->
                                let mutable result = Unchecked.defaultof<'T>
                                let success = try result <- field.Parser token :?> 'T ; true with _ -> false
                                if success then
                                    args.Add result
                                    if not isFirst then state.Reader.MoveNext()
                                    gather false
                                elif isFirst then
                                    match argInfo.UnrecognizedGatherParam.Value with
                                    | Some ugp -> aggregator.AppendResult ugp token [|token|]
                                    | None when state.IgnoreUnrecognizedArgs -> aggregator.AppendUnrecognized token
                                    | None -> error argInfo ErrorCode.CommandLine "unrecognized argument: '%s'." token
                            | _ -> ()

                        do gather true
                        match Seq.toList args with
                        | [] -> () ; false
                        | list -> aggregator.AppendResult mcp mcp.Name.Value [| list |] ; true }

            | paramInfo -> arguExn "internal error. MainCommand has param representation %A" paramInfo

        | _ ->

        match argInfo.UnrecognizedGatherParam.Value with
        | Some ugp -> aggregator.AppendResult ugp token [|token|]
        | None when state.IgnoreUnrecognizedArgs -> aggregator.AppendUnrecognized token
        | None -> error argInfo ErrorCode.CommandLine "unrecognized argument: '%s'." token

    | GroupedParams(_, switches) ->
        for sw in switches do
            let caseInfo = argInfo.CliParamIndex.Value.[sw]
            match caseInfo.ParameterInfo.Value with
            | Primitives [||] -> aggregator.AppendResult caseInfo sw [||]
            | OptionalParam _ -> aggregator.AppendResult caseInfo sw [|None|]
            | _ -> error argInfo ErrorCode.CommandLine "argument '%s' cannot be grouped with other switches." sw

    | CliParam(_, _, caseInfo, Assignment(name,sep,_)) when caseInfo.Arity <> 1 ->
        error argInfo ErrorCode.CommandLine "invalid CLI syntax '%s%s<param>'." name sep

    | CliParam(token, name, caseInfo, assignment) ->
        match caseInfo.ParameterInfo.Value with
        | Primitives [|field|] when caseInfo.IsCustomAssignment ->
            match assignment with
            | NoAssignment -> error argInfo ErrorCode.CommandLine "argument '%s' missing an assignment." name
            | Assignment(_,_,eqp) ->
                let argument =
                    try field.Parser eqp
                    with _ -> error argInfo ErrorCode.CommandLine "argument '%s' is assigned invalid value, should be <%s>." token field.Description

                aggregator.AppendResult caseInfo name [| argument |]

        | Primitives [|kf;vf|] when caseInfo.IsCustomAssignment ->
            match state.Reader.GetNextToken true argInfo with
            | UnrecognizedOrArgument token ->
                match caseInfo.AssignmentParser.Value token with
                | Assignment(key,_,value) ->
                    let k =
                        try kf.Parser key
                        with _ -> error argInfo ErrorCode.CommandLine "argument '%s' was given invalid key '%s', should be <%s>." state.Reader.CurrentSegment token kf.Description

                    let v =
                        try vf.Parser value
                        with _ -> error argInfo ErrorCode.CommandLine "argument '%s' was given invalid value assignment '%s', should be <%s>." state.Reader.CurrentSegment token vf.Description

                    aggregator.AppendResult caseInfo name [|k;v|]
                    state.Reader.MoveNext()

                | NoAssignment ->
                    error argInfo ErrorCode.CommandLine "argument '%s' must be followed by assignment '%s%s%s'."
                        caseInfo.Name.Value kf.Description caseInfo.CustomAssignmentSeparator.Value.Value vf.Description

            | CliParam(token,name,_,Assignment _) ->
                error argInfo ErrorCode.CommandLine "argument '%s' was given invalid key name '%s' in '%s'."
                    state.Reader.CurrentSegment name token
            | _ ->
                error argInfo ErrorCode.CommandLine "argument '%s' must be followed by assignment '%s%s%s'."
                    caseInfo.Name.Value kf.Description caseInfo.CustomAssignmentSeparator.Value.Value vf.Description

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
                aggregator.AppendResult caseInfo name fields

            parseSingleParameter ()
            if caseInfo.IsRest.Value then
                while not state.Reader.IsCompleted do
                    parseSingleParameter ()

        | OptionalParam(existential, field) when caseInfo.IsCustomAssignment ->
            let optArgument = existential.Accept { new IFunc<obj> with
                member __.Invoke<'T> () =
                    match assignment with
                    | NoAssignment -> Option<'T>.None :> obj
                    | Assignment(_,_,eqp) ->
                        let argument =
                            try field.Parser eqp
                            with _ ->
                                error argInfo ErrorCode.CommandLine "argument '%s' is assigned invalid value, should be <%s>."
                                    token field.Description

                        argument :?> 'T |> Some :> obj }

            aggregator.AppendResult caseInfo name [| optArgument |]

        | OptionalParam(existential, field) ->
            let optArgument = existential.Accept { new IFunc<obj> with
                member __.Invoke<'T> () =
                    match state.Reader.GetNextToken true argInfo with
                    | UnrecognizedOrArgument tok ->
                        let argument = try Some(field.Parser tok :?> 'T) with _ -> None
                        match argument with Some _ -> state.Reader.MoveNext() | None -> ()
                        argument :> obj

                    | _ -> Option<'T>.None :> obj }

            aggregator.AppendResult caseInfo name [| optArgument |]

        | ListParam(existential, field) ->
            let listArg = existential.Accept { new IFunc<obj> with
                member __.Invoke<'T>() =
                    let args = new ResizeArray<'T> ()
                    let rec gather () =
                        match state.Reader.GetNextToken true argInfo with
                        | UnrecognizedOrArgument token ->
                            let mutable result = Unchecked.defaultof<'T>
                            let isSuccess = try result <- field.Parser token :?> 'T ; true with _ -> false
                            if isSuccess then
                                args.Add result
                                state.Reader.MoveNext()
                                gather()
                        | _ -> ()

                    do gather ()
                    Seq.toList args :> obj
            }

            aggregator.AppendResult caseInfo name [| listArg |]

        | SubCommand (existential, nestedUnion, _) ->
            let nestedResults = parseCommandLineInner state nestedUnion
            let result =
                existential.Accept { new ITemplateFunc<obj> with
                    member __.Invoke<'Template when 'Template :> IArgParserTemplate> () =
                        new ParseResults<'Template>(nestedUnion, nestedResults, state.ProgramName, state.Description, state.UsageStringCharWidth, state.Exiter) :> obj }

            aggregator.AppendResult caseInfo name [|result|]

        | NullarySubCommand ->
            aggregator.AppendResult caseInfo name [||]

and private parseCommandLineInner (state : CliParseState) (argInfo : UnionArgInfo) =
    let results = state.ResultStack.CreateNextAggregator argInfo
    while not state.Reader.IsCompleted do parseCommandLinePartial state argInfo results
    if not results.IsSubCommandDefined && argInfo.IsRequiredSubcommand.Value then
        error argInfo ErrorCode.CommandLine "no valid subcommand has been specified."
    results.ToUnionParseResults()

/// <summary>
///     Parse the entire command line
/// </summary>
and parseCommandLine (argInfo : UnionArgInfo) (programName : string) (description : string option) (width : int) (exiter : IExiter)
                        (raiseOnUsage : bool) (ignoreUnrecognized : bool) (inputs : string []) =
    let state = {
        Reader = new CliTokenReader(inputs)
        ProgramName = programName
        ResultStack = new CliParseResultAggregatorStack(argInfo)
        Description = description
        UsageStringCharWidth = width
        RaiseOnUsage = raiseOnUsage
        IgnoreUnrecognizedArgs = ignoreUnrecognized
        Exiter = exiter
    }

    parseCommandLineInner state argInfo
