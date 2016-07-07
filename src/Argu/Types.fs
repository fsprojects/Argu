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
    | Unspecified   = 0
    | First         = 1
    | Last          = 2

/// Exception raised by Argu
type ArguException internal (message : string) =
    inherit Exception(message)

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
type ProcessExiter() =
    interface IExiter with
        member __.Name = "Process Exiter"
        member __.Exit(msg : string, errorCode : ErrorCode) =
            let writer = if errorCode = ErrorCode.HelpText then Console.Out else Console.Error
            writer.WriteLine msg
            writer.Flush()
            exit (int errorCode)

/// Abstract key/value configuration reader
type IConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// Gets value corresponding to supplied key
    abstract GetValue : key:string -> string

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
        Name : string
        /// Union case reflection identifier
        UnionCaseInfo : UnionCaseInfo
        /// Type of argument parser
        ArgumentType : ArgumentType

        /// head element denotes primary command line arg
        CommandLineNames : string list
        /// name used in AppSettings
        AppSettingsName : string option

        /// Description of the parameter
        Description : string list

        /// AppSettings parameter separator
        AppSettingsSeparators : string list
        /// AppSettings parameter split options
        AppSettingsSplitOptions : StringSplitOptions

        /// Mandated Cli position for the argument
        CliPosition : CliPosition
        /// If specified, should consume remaining tokens from the CLI
        IsRest : bool
        /// Separator token used for EqualsAssignment syntax; e.g. '=' forces '--param=arg' syntax
        CustomAssignmentSeparator : string option
        /// If specified, multiple parameters can be added in AppSettings in CSV form.
        AppSettingsCSV : bool
        /// Fails if no argument of this type is specified
        IsMandatory : bool
        /// Specifies that argument should be specified at most once in CLI
        IsUnique : bool
        /// Hide from Usage
        IsHidden : bool
        /// Declares that the parameter should gather any unrecognized CLI params
        IsGatherUnrecognized : bool
        /// Combine AppSettings with CLI inputs
        GatherAllSources : bool
    }