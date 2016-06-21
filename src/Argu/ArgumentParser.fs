namespace Argu

open System
open System.Configuration
open System.Reflection
open System.Xml
open System.Xml.Linq

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

exception private HelpText

/// The Argu type generates an argument parser given a type argument
/// that is an F# discriminated union. It can then be used to parse command line arguments
/// or XML configuration.
[<NoEquality; NoComparison; Sealed; AutoSerializable(false)>]
type ArgumentParser<'Template when 'Template :> IArgParserTemplate> (?usageText : string) =
    static let argInfoL = lazy(preComputeUnionArgInfo typeof<'Template>)
    let argInfo = argInfoL.Value

    let mkUsageString msgOpt = printUsage argInfo msgOpt |> String.build

    let (|ParserExn|_|) (e : exn) =
        match e with
        // do not display usage for App.Config-only parameter errors
        | ParseError (msg, id, Some aI) when aI.NoCommandLine -> Some(id, msg)
        | ParseError (msg, id, _) -> Some (id, mkUsageString (Some msg))
        | HelpText -> Some (ErrorCode.HelpText, mkUsageString usageText)
        | _ -> None

    /// <summary>Parse command line arguments only.</summary>
    /// <param name="inputs">The command line input. Taken from System.Environment if not specified.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    /// <param name="ignoreUnrecognized">Ignore CLI arguments that do not match the schema. Defaults to false.</param>
    /// <param name="raiseOnUsage">Treat '--help' parameters as parse errors. Defaults to true.</param>
    member __.ParseCommandLine (?inputs : string [], ?errorHandler: IExiter, ?ignoreMissing, ?ignoreUnrecognized, ?raiseOnUsage) : ParseResult<'Template> =
        let ignoreMissing = defaultArg ignoreMissing false
        let ignoreUnrecognized = defaultArg ignoreUnrecognized false
        let raiseOnUsage = defaultArg raiseOnUsage true
        let errorHandler = match errorHandler with Some e -> e | None -> ExceptionExiter.ArgumentExceptionExiter()
        let inputs = match inputs with None -> getEnvironmentCommandLineArgs () | Some args -> args

        try
            let cliResults = parseCommandLine argInfo errorHandler ignoreUnrecognized inputs

            if cliResults.IsUsageRequested && raiseOnUsage then raise HelpText

            let ignoreMissing = (cliResults.IsUsageRequested && not raiseOnUsage) || ignoreMissing

            let results = postProcessResults argInfo ignoreMissing None (Some cliResults)

            new ParseResult<'Template>(argInfo, results, mkUsageString, errorHandler)
        with
        | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

    /// <summary>Parse AppSettings section of XML configuration only.</summary>
    /// <param name="xmlConfigurationFile">If specified, parse AppSettings configuration from given xml configuration file.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    member __.ParseAppSettings (?xmlConfigurationFile : string, ?errorHandler: IExiter, ?ignoreMissing) : ParseResult<'Template> =
        let ignoreMissing = defaultArg ignoreMissing false
        let errorHandler = 
            match errorHandler with
            | None -> ExceptionExiter.ArgumentExceptionExiter()
            | Some eh -> eh

        try
            let appSettingsResults = parseAppSettings (getConfigurationManagerReader xmlConfigurationFile) argInfo
            let results = postProcessResults argInfo ignoreMissing (Some appSettingsResults) None

            new ParseResult<'Template>(argInfo, results, mkUsageString, errorHandler)
        with
        | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

    /// <summary>Parse AppSettings section of XML configuration of given assembly.</summary>
    /// <param name="assembly">assembly to get application configuration from.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    member s.ParseAppSettings(assembly : Assembly, ?errorHandler : IExiter, ?ignoreMissing) =
        let configFile = assembly.Location + ".config"
        s.ParseAppSettings(xmlConfigurationFile = configFile, ?errorHandler = errorHandler, ?ignoreMissing = ignoreMissing)

    /// <summary>Parse both command line args and AppSettings section of XML configuration.
    ///          Results are merged with command line args overriding XML config.</summary>
    /// <param name="inputs">The command line input. Taken from System.Environment if not specified.</param>
    /// <param name="xmlConfigurationFile">If specified, parse AppSettings configuration from given configuration file.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    /// <param name="ignoreUnrecognized">Ignore CLI arguments that do not match the schema. Defaults to false.</param>
    /// <param name="raiseOnUsage">Treat '--help' parameters as parse errors. Defaults to false.</param>
    member __.Parse (?inputs : string [], ?xmlConfigurationFile : string, ?errorHandler : IExiter, ?ignoreMissing, ?ignoreUnrecognized, ?raiseOnUsage) : ParseResult<'Template> =
        let ignoreMissing = defaultArg ignoreMissing false
        let ignoreUnrecognized = defaultArg ignoreUnrecognized false
        let raiseOnUsage = defaultArg raiseOnUsage true
        let errorHandler = match errorHandler with Some e -> e | None -> ExceptionExiter.ArgumentExceptionExiter()
        let inputs = match inputs with None -> getEnvironmentCommandLineArgs () | Some args -> args

        try
            let appSettingsResults = parseAppSettings (getConfigurationManagerReader xmlConfigurationFile) argInfo
            let cliResults = parseCommandLine argInfo errorHandler ignoreUnrecognized inputs
            if cliResults.IsUsageRequested && raiseOnUsage then raise HelpText

            let results = postProcessResults argInfo ignoreMissing (Some appSettingsResults) (Some cliResults)

            new ParseResult<'Template>(argInfo, results, mkUsageString, errorHandler)
        with
        | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

    /// <summary>
    ///     Converts a sequence of template argument inputs into a ParseResult instance
    /// </summary>
    /// <param name="inputs">Argument input sequence.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
    member __.ToParseResult (inputs : seq<'Template>, ?errorHandler : IExiter) : ParseResult<'Template> =
        let errorHandler = match errorHandler with Some e -> e | None -> ExceptionExiter.ArgumentExceptionExiter()
        mkParseResultFromValues argInfo errorHandler mkUsageString inputs

    /// <summary>Returns the usage string.</summary>
    /// <param name="message">The message to be displayed on top of the usage string.</param>
    member __.Usage (?message : string) : string = mkUsageString message

    /// <summary>
    ///     Gets the DU tag representation for given argument
    /// </summary>
    /// <param name="value">Argument instance.</param>
    member __.GetTag(value : 'Template) : int = argInfo.TagReader.Value (value :> obj)

    /// <summary>Prints command line syntax. Useful for generating documentation.</summary>
    member __.PrintCommandLineSyntax () : string =
        printCommandLineSyntax argInfo |> String.build

    /// <summary>Prints parameters in command line format. Useful for argument string generation.</summary>
    member __.PrintCommandLine (args : 'Template list) : string [] =
        printCommandLineArgs argInfo (Seq.cast args) |> Seq.toArray

    /// <summary>Prints parameters in command line format. Useful for argument string generation.</summary>
    member __.PrintCommandLineFlat (args : 'Template list) : string =
        __.PrintCommandLine args
        |> Seq.map (sprintf "\"%s\"")
        |> String.concat " "

    /// <summary>Prints parameters in App.Config format.</summary>
    /// <param name="args">The parameters that fill out the XML document.</param>
    /// <param name="printComments">Print XML comments over every configuration entry.</param>
    member __.PrintAppSettings (args : 'Template list, ?printComments) : string =
        let printComments = defaultArg printComments true
        let xmlDoc = printAppSettings argInfo printComments args
        use writer = { new System.IO.StringWriter() with member __.Encoding = System.Text.Encoding.UTF8 }
        xmlDoc.Save writer
        writer.Flush()
        writer.ToString()

/// <summary>
///     Argu static methods
/// </summary>
type ArgumentParser =
        
    /// <summary>
    ///     Create a new argument parsing scheme using given 'Template type
    ///     which must be an F# Discriminated Union.
    /// </summary>
    /// <param name="usageText">Specify a usage text to prefixed at the '--help' output.</param>
    static member Create<'Template when 'Template :> IArgParserTemplate>(?usageText : string) =
        new ArgumentParser<'Template>(?usageText = usageText)


[<AutoOpen>]
module ArgumentParserUtils =
    
    /// converts a sequence of inputs to a ParseResult instance
    let toParseResults (inputs : seq<'Template>) : ParseResult<'Template> =
        ArgumentParser.Create<'Template>().ToParseResult(inputs)

    /// gets the DU tag representation of given argument instance
    let tagOf (input : 'Template) : int =
        ArgumentParser.Create<'Template>().GetTag input