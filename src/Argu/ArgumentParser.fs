namespace Argu

open System
open System.Configuration
open System.Reflection
open System.Xml
open System.Xml.Linq

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns


/// The Argu type generates an argument parser given a type argument
/// that is an F# discriminated union. It can then be used to parse command line arguments
/// or XML configuration.
type ArgumentParser<'Template when 'Template :> IArgParserTemplate> internal (?usageText : string) =
    do 
        if not <| FSharpType.IsUnion(typeof<'Template>, bindingFlags = allBindings) then
            invalidArg typeof<'Template>.Name "Argu: template type inaccessible or not F# DU."

    let argInfo =
        FSharpType.GetUnionCases(typeof<'Template>, bindingFlags = allBindings)
        |> Seq.map preComputeArgInfo
        |> Seq.sortBy (fun a -> a.UCI.Tag)
        |> Seq.toList

    do checkForConflictingParameters argInfo

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
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    /// <param name="ignoreUnrecognized">Ignore CLI arguments that do not match the schema. Defaults to false.</param>
    /// <param name="raiseOnUsage">Treat '--help' parameters as parse errors. Defaults to true.</param>
    member s.ParseCommandLine (?inputs : string [], ?errorHandler: IExiter, ?ignoreMissing, ?ignoreUnrecognized, ?raiseOnUsage) =
        let ignoreMissing = defaultArg ignoreMissing false
        let ignoreUnrecognized = defaultArg ignoreUnrecognized false
        let raiseOnUsage = defaultArg raiseOnUsage true
        let errorHandler = defaultArg errorHandler <| ExceptionExiter.ArgumentExceptionExiter()
        let inputs = match inputs with None -> getEnvironmentCommandLineArgs () | Some args -> args

        try
            let cliResults = parseCommandLine<'Template> clArgIdx ignoreUnrecognized inputs

            if cliResults.HelpArgs > 0 && raiseOnUsage then raise HelpText

            let ignoreMissing = (cliResults.HelpArgs > 0 && not raiseOnUsage) || ignoreMissing

            let results = combine argInfo ignoreMissing None (Some cliResults.ParseResults)

            ParseResults<_>(s, errorHandler, results, cliResults.HelpArgs > 0)
        with
        | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

    /// <summary>Parse AppSettings section of XML configuration only.</summary>
    /// <param name="xmlConfigurationFile">If specified, parse AppSettings configuration from given xml configuration file.</param>
    /// <param name="errorHandler">The implementation of IExiter used for error handling. ArgumentException is default.</param>
    /// <param name="ignoreMissing">Ignore errors caused by the Mandatory attribute. Defaults to false.</param>
    member s.ParseAppSettings (?xmlConfigurationFile : string, ?errorHandler: IExiter, ?ignoreMissing) =
        let ignoreMissing = defaultArg ignoreMissing false
        let errorHandler = 
            match errorHandler with
            | None -> ExceptionExiter.ArgumentExceptionExiter()
            | Some eh -> eh

        try
            let appSettingsResults = parseAppSettings xmlConfigurationFile argInfo
            let results = combine argInfo ignoreMissing (Some appSettingsResults) None

            ParseResults<_>(s, errorHandler, results, false)
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
    member s.Parse (?inputs : string [], ?xmlConfigurationFile : string, ?errorHandler : IExiter, ?ignoreMissing, ?ignoreUnrecognized, ?raiseOnUsage) =
        let ignoreMissing = defaultArg ignoreMissing false
        let ignoreUnrecognized = defaultArg ignoreUnrecognized false
        let raiseOnUsage = defaultArg raiseOnUsage true
        let errorHandler = defaultArg errorHandler <| ExceptionExiter.ArgumentExceptionExiter()
        let inputs = match inputs with None -> getEnvironmentCommandLineArgs () | Some args -> args

        try
            let appSettingsResults = parseAppSettings xmlConfigurationFile argInfo
            let cliResults = parseCommandLine<'Template> clArgIdx ignoreUnrecognized inputs
            if cliResults.HelpArgs > 0 && raiseOnUsage then raise HelpText

            let results = combine argInfo ignoreMissing (Some appSettingsResults) (Some cliResults.ParseResults)

            ParseResults<_>(s, errorHandler, results, cliResults.HelpArgs > 0)
        with
        | ParserExn (id, msg) -> errorHandler.Exit (msg, int id)

    /// <summary>Returns the usage string.</summary>
    /// <param name="message">The message to be displayed on top of the usage string.</param>
    member __.Usage (?message : string) : string = printUsage message argInfo

    /// <summary>Prints command line syntax. Useful for generating documentation.</summary>
    member __.PrintCommandLineSyntax () : string =
        printCommandLineSyntax argInfo

    /// <summary>Prints parameters in command line format. Useful for argument string generation.</summary>
    member __.PrintCommandLine (args : 'Template list) : string [] =
        printCommandLineArgs argInfo args

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