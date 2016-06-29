namespace Argu

open System
open System.Collections.Generic
open System.Reflection

open FSharp.Quotations
open FSharp.Reflection

/// The Argu type generates an argument parser given a type argument
/// that is an F# discriminated union. It can then be used to parse command line arguments
/// or XML configuration.
[<AbstractClass; NoEquality; NoComparison; AutoSerializable(false)>]
type ArgumentParser internal (argInfo : UnionArgInfo, _programName : string, helpTextMessage : string option,
                                _usageStringCharacterWidth : int, errorHandler : IExiter) =

    /// Gets the help flags specified for the CLI parser
    member __.HelpFlags = argInfo.HelpParam.Flags
    /// Gets the help description specified for the CLI parser
    member __.HelpDescription = argInfo.HelpParam.Description
    /// Gets the message that will be displayed at the top of the help text
    member __.HelpTextMessage = helpTextMessage
    /// Returns true if parser corresponds to a subcommand
    member __.IsSubCommandParser = argInfo.TryGetParent() |> Option.isSome
    /// If subcommand parser, gets parent argument metadata
    member __.ParentInfo = argInfo.TryGetParent() |> Option.map (fun p -> p.ToArgumentCaseInfo())
    /// Gets the default error handler used by the instance
    member __.ErrorHandler = errorHandler

    /// Gets metadata for all union cases used by parser
    member __.GetArgumentCases() = 
        argInfo.Cases 
        |> Seq.map (fun p -> p.ToArgumentCaseInfo()) 
        |> Seq.toList

    /// Gets all subcommand parsers for given parser
    member __.GetSubCommandParsers() =
        argInfo.Cases
        |> Seq.choose (fun cs -> match cs.ParameterInfo with NestedUnion(e,nu) -> Some(e,nu) | _ -> None)
        |> Seq.map (fun (e,nu) -> 
            e.Accept { 
                new ITemplateFunc<ArgumentParser> with
                    member __.Invoke<'Template when 'Template :> IArgParserTemplate> () =
                        new ArgumentParser<'Template>(nu, _programName, helpTextMessage, _usageStringCharacterWidth, errorHandler) :> _ 
            })
        |> Seq.toList

    /// <summary>Formats a usage string for the argument parser.</summary>
    /// <param name="message">The message to be displayed on top of the usage string.</param>
    /// <param name="programName">Override the default program name settings.</param>
    /// <param name="usageStringCharacterWidth">Text width used when formatting the usage string.</param>
    member __.PrintUsage (?message : string, ?programName : string, ?usageStringCharacterWidth : int) : string = 
        let programName = defaultArg programName _programName
        let usageStringCharacterWidth = defaultArg usageStringCharacterWidth _usageStringCharacterWidth
        mkUsageString argInfo programName usageStringCharacterWidth message |> StringExpr.build

    /// <summary>
    ///     Prints command line syntax. Useful for generating documentation.
    /// </summary>
    /// <param name="programName">Program name identifier placed at start of syntax string</param>
    /// <param name="usageStringCharacterWidth">Text width used when formatting the usage string.</param>
    member __.PrintCommandLineSyntax (?programName : string, ?usageStringCharacterWidth : int) : string =
        let programName = defaultArg programName _programName
        let usageStringCharacterWidth = defaultArg usageStringCharacterWidth _usageStringCharacterWidth
        mkCommandLineSyntax argInfo "" usageStringCharacterWidth programName |> StringExpr.build

    /// <summary>
    ///     Enables access to the typed API of an ArgumentParser 
    ///     when template type is unknown.
    /// </summary>
    /// <param name="visitor">Visitor used to access the parser.</param>
    abstract Accept : visitor:IArgumentParserVisitor<'R> -> 'R

/// The Argu type generates an argument parser given a type argument
/// that is an F# discriminated union. It can then be used to parse command line arguments
/// or XML configuration.
and [<Sealed; NoEquality; NoComparison; AutoSerializable(false)>]
    ArgumentParser<'Template when 'Template :> IArgParserTemplate> 
        internal (argInfo : UnionArgInfo, _programName : string, helpTextMessage : string option, 
                    _usageStringCharacterWidth : int, errorHandler : IExiter) =

    inherit ArgumentParser(argInfo, _programName, helpTextMessage, _usageStringCharacterWidth, errorHandler)

    // memoize parser generation for given template type
    static let argInfoLazy = lazy(preComputeUnionArgInfo<'Template> ())

    let mkUsageString argInfo msgOpt = mkUsageString argInfo _programName _usageStringCharacterWidth msgOpt |> StringExpr.build

    let (|ParserExn|_|) (e : exn) =
        match e with
        // do not display usage for App.Config parameter errors
        | ParseError (msg, ErrorCode.AppSettings, _) -> Some(ErrorCode.AppSettings, msg)
        | ParseError (msg, id, aI) -> Some (id, mkUsageString aI (Some msg))
        | HelpText aI -> Some (ErrorCode.HelpText, mkUsageString aI helpTextMessage)
        | _ -> None

    /// <summary>
    ///     Creates a new parser instance based on supplied F# union template.
    /// </summary>
    /// <param name="programName">Program identifier, e.g. 'cat'. Defaults to the current executable name.</param>
    /// <param name="helpTextMessage">Message that will be displayed at the top of the help text.</param>
    /// <param name="usageStringCharacterWidth">Text width used when formatting the usage string. Defaults to 80 chars.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. Exception is default.</param>
    new (?programName : string, ?helpTextMessage : string, ?usageStringCharacterWidth : int, ?errorHandler : IExiter) =
        let usageStringCharacterWidth = defaultArg usageStringCharacterWidth 80
        let programName = match programName with Some pn -> pn | None -> currentProgramName.Value
        let errorHandler = match errorHandler with Some e -> e  | None -> new ExceptionExiter() :> _
        new ArgumentParser<'Template>(argInfoLazy.Value, programName, helpTextMessage, usageStringCharacterWidth, errorHandler)

    /// <summary>Parse command line arguments only.</summary>
    /// <param name="inputs">The command line input. Taken from System.Environment if not specified.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    /// <param name="ignoreUnrecognized">Ignore CLI arguments that do not match the schema. Defaults to false.</param>
    /// <param name="raiseOnUsage">Treat '--help' parameters as parse errors. Defaults to true.</param>
    member __.ParseCommandLine (?inputs : string [], ?ignoreMissing, ?ignoreUnrecognized, ?raiseOnUsage) : ParseResult<'Template> =
        let ignoreMissing = defaultArg ignoreMissing false
        let ignoreUnrecognized = defaultArg ignoreUnrecognized false
        let raiseOnUsage = defaultArg raiseOnUsage true
        let inputs = match inputs with None -> getEnvironmentCommandLineArgs () | Some args -> args

        try
            let cliResults = parseCommandLine argInfo _programName helpTextMessage _usageStringCharacterWidth errorHandler raiseOnUsage ignoreUnrecognized inputs
            let ignoreMissing = (cliResults.IsUsageRequested && not raiseOnUsage) || ignoreMissing
            let results = postProcessResults argInfo ignoreMissing None (Some cliResults)

            new ParseResult<'Template>(argInfo, results, _programName, helpTextMessage, _usageStringCharacterWidth, errorHandler)

        with ParserExn (errorCode, msg) -> errorHandler.Exit (msg, errorCode)

    /// <summary>Parse arguments using specified configuration reader only. This defaults to the AppSettings configuration of the current process.</summary>
    /// <param name="configurationReader">Configuration reader used to source the arguments. Defaults to the AppSettings configuration of the current process.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    member __.ParseConfiguration (configurationReader : IConfigurationReader, ?ignoreMissing : bool) : ParseResult<'Template> =
        let ignoreMissing = defaultArg ignoreMissing false

        try
            let appSettingsResults = parseKeyValueConfig configurationReader argInfo
            let results = postProcessResults argInfo ignoreMissing (Some appSettingsResults) None

            new ParseResult<'Template>(argInfo, results, _programName, helpTextMessage, _usageStringCharacterWidth, errorHandler)

        with ParserExn (errorCode, msg) -> errorHandler.Exit (msg, errorCode)    

    /// <summary>Parse both command line args and supplied configuration reader.
    ///          Results are merged with command line args overriding configuration parameters.</summary>
    /// <param name="inputs">The command line input. Taken from System.Environment if not specified.</param>
    /// <param name="configurationReader">Configuration reader used to source the arguments. Defaults to the AppSettings configuration of the current process.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    /// <param name="ignoreUnrecognized">Ignore CLI arguments that do not match the schema. Defaults to false.</param>
    /// <param name="raiseOnUsage">Treat '--help' parameters as parse errors. Defaults to false.</param>
    member __.Parse (?inputs : string [], ?configurationReader : IConfigurationReader, ?ignoreMissing, ?ignoreUnrecognized, ?raiseOnUsage) : ParseResult<'Template> =
        let ignoreMissing = defaultArg ignoreMissing false
        let ignoreUnrecognized = defaultArg ignoreUnrecognized false
        let raiseOnUsage = defaultArg raiseOnUsage true
        let inputs = match inputs with None -> getEnvironmentCommandLineArgs () | Some args -> args
        let configurationReader = match configurationReader with Some c -> c | None -> ConfigurationReader.FromAppSettings()

        try
            let appSettingsResults = parseKeyValueConfig configurationReader argInfo
            let cliResults = parseCommandLine argInfo _programName helpTextMessage _usageStringCharacterWidth errorHandler raiseOnUsage ignoreUnrecognized inputs
            let results = postProcessResults argInfo ignoreMissing (Some appSettingsResults) (Some cliResults)

            new ParseResult<'Template>(argInfo, results, _programName, helpTextMessage, _usageStringCharacterWidth, errorHandler)

        with ParserExn (errorCode, msg) -> errorHandler.Exit (msg, errorCode)

    /// <summary>Parse AppSettings section of XML configuration only.</summary>
    /// <param name="xmlConfigurationFile">If specified, parse AppSettings configuration from given xml configuration file.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    [<Obsolete("Use ArgumentParser.ParseConfiguration method instead")>]
    member __.ParseAppSettings (?xmlConfigurationFile : string, ?ignoreMissing : bool) : ParseResult<'Template> =
        let configurationReader =
            match xmlConfigurationFile with
            | None -> ConfigurationReader.FromAppSettings()
            | Some f -> ConfigurationReader.FromAppSettingsFile(f)

        __.ParseConfiguration(configurationReader, ?ignoreMissing = ignoreMissing)

    /// <summary>Parse AppSettings section of XML configuration of given assembly.</summary>
    /// <param name="assembly">assembly to get application configuration from.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    [<Obsolete("Use ArgumentParser.ParseConfiguration method instead")>]
    member __.ParseAppSettings(assembly : Assembly, ?ignoreMissing : bool) =
        let configurationReader = ConfigurationReader.FromAppSettings(assembly)
        __.ParseConfiguration(configurationReader, ?ignoreMissing = ignoreMissing)

    /// <summary>
    ///     Converts a sequence of template argument inputs into a ParseResult instance
    /// </summary>
    /// <param name="inputs">Argument input sequence.</param>
    member __.ToParseResult (inputs : seq<'Template>) : ParseResult<'Template> =
        mkParseResultFromValues argInfo errorHandler _usageStringCharacterWidth _programName helpTextMessage inputs

    /// <summary>
    ///     Gets a subparser associated with specific subcommand instance
    /// </summary>
    /// <param name="expr">Expression providing the subcommand union constructor.</param>
    member __.GetSubCommandParser (expr : Expr<ParseResult<'SubTemplate> -> 'Template>) : ArgumentParser<'SubTemplate> =
        let uci = expr2Uci expr
        let case = argInfo.Cases.[uci.Tag]
        match case.ParameterInfo with
        | NestedUnion (_,nestedUnion) -> 
            new ArgumentParser<'SubTemplate>(nestedUnion, _programName, helpTextMessage, _usageStringCharacterWidth, errorHandler)
        | _ -> arguExn "internal error when fetching subparser %O." uci

    /// <summary>
    ///     Gets the F# union tag representation for given argument
    /// </summary>
    /// <param name="value">Argument instance.</param>
    member __.GetTag(value : 'Template) : int = 
        argInfo.TagReader.Value (value :> obj)

    /// <summary>
    ///     Gets argument metadata for given argument instance.
    /// </summary>
    /// <param name="value">Argument instance.</param>
    member __.GetArgumentCaseInfo(value : 'Template) : ArgumentCaseInfo =
        let tag = argInfo.TagReader.Value (value :> obj)
        argInfo.Cases.[tag].ToArgumentCaseInfo()

    /// <summary>
    ///     Gets argument metadata for given union case constructor
    /// </summary>
    /// <param name="ctorExpr">Quoted union case constructor.</param>
    member __.GetArgumentCaseInfo(ctorExpr : Expr<'Fields -> 'Template>) : ArgumentCaseInfo =
        let uci = expr2Uci ctorExpr
        argInfo.Cases.[uci.Tag].ToArgumentCaseInfo()

    /// <summary>Prints parameters in command line format. Useful for argument string generation.</summary>
    member __.PrintCommandLineArguments (args : 'Template list) : string [] =
        mkCommandLineArgs argInfo (Seq.cast args) |> Seq.toArray

    /// <summary>Prints parameters in command line format. Useful for argument string generation.</summary>
    member __.PrintCommandLineArgumentsFlat (args : 'Template list) : string =
        __.PrintCommandLineArguments args |> flattenCliTokens

    /// <summary>Prints parameters in App.Config format.</summary>
    /// <param name="args">The parameters that fill out the XML document.</param>
    /// <param name="printComments">Print XML comments over every configuration entry.</param>
    member __.PrintAppSettingsArguments (args : 'Template list, ?printComments : bool) : string =
        let printComments = defaultArg printComments true
        let xmlDoc = mkAppSettingsDocument argInfo printComments args
        use writer = { new System.IO.StringWriter() with member __.Encoding = System.Text.Encoding.UTF8 }
        xmlDoc.Save writer
        writer.Flush()
        writer.ToString()


    override self.Accept visitor = visitor.Visit self

/// Rank-2 function used for accessing typed APIs of untyped parsers
and IArgumentParserVisitor<'R> =
    /// <summary>
    ///     Visit argument parser of generic type.
    /// </summary>
    /// <param name="parser">Supplied argument parser.</param>
    abstract Visit<'Template when 'Template :> IArgParserTemplate> : parser:ArgumentParser<'Template> -> 'R

/// <summary>
///     Argu static methods
/// </summary>
type ArgumentParser with
        
    /// <summary>
    ///     Create a new argument parsing scheme using given 'Template type
    ///     which must be an F# Discriminated Union.
    /// </summary>
    /// <param name="programName">Program identifier, e.g. 'cat'. Defaults to the current executable name.</param>
    /// <param name="helpTextMessage">Message that will be displayed at the top of the help text.</param>
    /// <param name="usageStringCharacterWidth">Text width used when formatting the usage string. Defaults to 80 chars.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. Exception is default.</param>
    static member Create<'Template when 'Template :> IArgParserTemplate>(?programName : string, ?helpTextMessage : string, ?usageStringCharacterWidth : int, ?errorHandler : IExiter) =
        new ArgumentParser<'Template>(?programName = programName, ?helpTextMessage = helpTextMessage, ?errorHandler = errorHandler, ?usageStringCharacterWidth = usageStringCharacterWidth)


[<AutoOpen>]
module ArgumentParserUtils =

    type ParseResult<'Template when 'Template :> IArgParserTemplate> with
        /// Gets the parser instance corresponding to the parse result
        member r.Parser =
            new ArgumentParser<'Template>(r.ArgInfo, r.ProgramName, r.Description, 
                                                r.CharacterWidth, r.ErrorHandler)

    /// converts a sequence of inputs to a ParseResult instance
    let toParseResults (inputs : seq<'Template>) : ParseResult<'Template> =
        ArgumentParser.Create<'Template>().ToParseResult(inputs)

    /// gets the F# union tag representation of given argument instance
    let tagOf (input : 'Template) : int =
        ArgumentParser.Create<'Template>().GetTag input