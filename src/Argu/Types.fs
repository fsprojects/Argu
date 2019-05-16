namespace Argu

open System
open FSharp.Reflection

/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage : string

/// Predefined CLI prefixes to be added
[<RequireQualifiedAccess>]
module CliPrefix =
    /// No Cli Prefix
    let [<Literal>] None = ""
    /// Single Dash prefix '-'
    let [<Literal>] Dash = "-"
    /// Double Dash prefix '--'
    let [<Literal>] DoubleDash = "--"

/// Source from which to parse arguments
[<Flags>]
type ParseSource =
    | None          = 0
    | AppSettings   = 1
    | CommandLine   = 2
    | All           = 3

/// Error codes reported by Argu
type ErrorCode =
    | HelpText = 0
    | AppSettings = 1
    | CommandLine = 2
    | PostProcess = 3

/// Cli Position required to place argument
type CliPosition =
    | First         = 1
    | Unspecified   = 2
    | Last          = 3

/// Exception raised by Argu
type ArguException internal (message : string, exn : Exception) =
    inherit Exception(message, exn)
    internal new(message) = ArguException(message, null)

/// Parse exception raised by Argu
type ArguParseException internal (message : string, errorCode : ErrorCode) =
    inherit ArguException(message)
    member __.ErrorCode = errorCode

/// An interface for error handling in the argument parser
type IExiter =
    /// IExiter identifier
    abstract Name : string
    /// handle error of given message and error code
    abstract Exit : msg : string * errorCode : ErrorCode -> 'T

/// Handles argument parser errors by raising an exception
type ExceptionExiter() =
    interface IExiter with
        member __.Name = "ArguException Exiter"
        member __.Exit(msg, errorCode) = raise (new ArguParseException(msg, errorCode))

/// Handles argument parser errors by exiting the process
/// after printing a parse error.
type ProcessExiter(colorizerOption : (ErrorCode -> ConsoleColor option) option) =
    let colorize errorCode =
        match colorizerOption |> Option.bind (fun clr -> clr errorCode) with
        | None -> null
        | Some color ->
            let previous = Console.ForegroundColor
            Console.ForegroundColor <- color
            { new IDisposable with member __.Dispose() = Console.ForegroundColor <- previous }

    // Note: this ctor is required to preserve binary compatibility with < 3.7
    new () = ProcessExiter(None)
    new (colorizer : (ErrorCode -> ConsoleColor option)) = ProcessExiter(Some colorizer)

    interface IExiter with
        member __.Name = "Process Exiter"
        member __.Exit(msg : string, errorCode : ErrorCode) =
            let writer = if errorCode = ErrorCode.HelpText then Console.Out else Console.Error
            do
                use _d = colorize errorCode
                writer.WriteLine msg
                writer.Flush()

            exit (int errorCode)

/// Argument parameter type identifier
type ArgumentType =
    /// Argument specifies primitive parameters like strings or integers
    | Primitive  = 1
    /// Argument specifies an optional parameter which is primitive
    | Optional   = 2
    /// Argument specifies a list of parameters of specific primitive type
    | List       = 3
    /// Argument specifies a subcommand
    | SubCommand = 4

/// Union argument metadata
[<NoEquality; NoComparison>]
type ArgumentCaseInfo =
    {
        /// Human readable name identifier
        Name : Lazy<string>
        /// Union case reflection identifier
        UnionCaseInfo : UnionCaseInfo
        /// Type of argument parser
        ArgumentType : ArgumentType

        /// head element denotes primary command line arg
        CommandLineNames : Lazy<string list>
        /// name used in AppSettings
        AppSettingsName : Lazy<string option>

        /// Description of the parameter
        Description : Lazy<string>

        /// AppSettings parameter separator
        AppSettingsSeparators : string list
        /// AppSettings parameter split options
        AppSettingsSplitOptions : StringSplitOptions

        /// Mandated Cli position for the argument
        CliPosition : Lazy<CliPosition>
        /// Specifies that this argument is the main CLI command
        IsMainCommand : bool
        /// If specified, should consume remaining tokens from the CLI
        IsRest : Lazy<bool>
        /// Separator token used for EqualsAssignment syntax; e.g. '=' forces '--param=arg' syntax
        CustomAssignmentSeparator : Lazy<string option>
        /// If specified, multiple parameters can be added in AppSettings in CSV form.
        AppSettingsCSV : Lazy<bool>
        /// Fails if no argument of this type is specified
        IsMandatory : Lazy<bool>
        /// Specifies that argument should be specified at most once in CLI
        IsUnique : Lazy<bool>
        /// Hide from Usage
        IsHidden : Lazy<bool>
        /// Declares that the parameter should gather any unrecognized CLI params
        IsGatherUnrecognized : Lazy<bool>
        /// Combine AppSettings with CLI inputs
        GatherAllSources : Lazy<bool>
    }
