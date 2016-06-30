namespace Argu

open System
open FSharp.Reflection

// Attribute declarations

[<AutoOpen>]
module ArguAttributes =

    /// Parse comma separated values in AppSettings
    [<Obsolete("Please use list parameters instead.")>]
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type ParseCSVAttribute () = inherit Attribute ()

    /// Consume all remaining command line arguments.
    [<Obsolete("Please use list parameters instead.")>]
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type RestAttribute () = inherit Attribute ()

    /// Hide from command line argument documentation.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type HiddenAttribute () = inherit Attribute ()

    /// Demands at least one parsed result for this branch; an exception is raised otherwise.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type MandatoryAttribute () = inherit Attribute ()

    /// Demands that the argument should be specified at most once; an exception is raised otherwise.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type UniqueAttribute () = inherit Attribute ()

    /// Demands that the argument should be specified exactly once; an exception is raised otherwise.
    /// Equivalent to attaching both the Mandatory and Unique attribute on the parameter.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type ExactlyOnceAttribute () = inherit Attribute ()

    /// Denotes that the given argument should be inherited in the scope of any subcommands
    /// in the current argument template
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type InheritAttribute() = inherit Attribute()

    /// Demands that at least one subcommand is specified in the CLI; an exception is raised otherwise.
    [<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
    type RequireSubcommandAttribute () = inherit Attribute()

    /// Gathers all parsed results from both AppSettings and command line.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type GatherAllSourcesAttribute () = inherit Attribute ()

    /// Disable command line parsing for this branch.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type NoCommandLineAttribute () = inherit Attribute ()

    /// Disable AppSettings parsing for this branch.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type NoAppSettingsAttribute () = inherit Attribute ()

    /// Specifies replacement flags for help params
    [<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
    type HelpFlagsAttribute ([<ParamArray>] names : string []) = 
        inherit Attribute()
        member __.Names = names

    /// Specifies that the union should not take --help parameters
    [<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
    type DisableHelpFlagsAttribute () = inherit HelpFlagsAttribute ()

    /// Specifies a custom help param description for the usage string
    [<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
    type HelpDescriptionAttribute (description : string) =
        inherit Attribute()
        member __.Description = description

    /// Argument can only be placed at the beginning of the command line.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type FirstAttribute () = inherit Attribute ()

    /// Print F# 3.1 field labels in usage string.
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    [<Obsolete("Argu 3.0 prints union labels by default. Please remove this attribute.")>]
    type PrintLabelsAttribute () = inherit Attribute ()

    /// Use '--param=arg' or '--param key=value' assignment syntax in CLI.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type EqualsAssignmentAttribute () = inherit Attribute ()

    /// Sets a custom command line name.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type CustomCommandLineAttribute (name : string) =
        inherit Attribute ()
        member __.Name = name

    /// Sets alternative command line names.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
    type AltCommandLineAttribute ([<ParamArray>] names :string []) = 
        inherit Attribute ()
        member __.Names = names

    /// Sets a custom Configuration parsing key name.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
    type CustomAppSettingsAttribute (name : string) =
        inherit Attribute ()
        member __.Name = name

    /// Specify a custom value separator in Configuration parsing parameters
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
    type AppSettingsSeparatorAttribute ([<ParamArray>] separators : string [], splitOptions : StringSplitOptions) =
        inherit Attribute()
        new (separator : char) = new AppSettingsSeparatorAttribute([|string separator|], StringSplitOptions.None)
        member __.Separators = separators
        member __.SplitOptions = splitOptions

    /// Specifies a custom prefix for auto generated CLI names.
    [<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Class, AllowMultiple = false)>]
    type CliPrefixAttribute(prefix : string) = 
        inherit Attribute() 
        member __.Prefix = prefix

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

type ErrorCode =
    | HelpText = 0
    | AppSettings = 1
    | CommandLine = 2
    | PostProcess = 3

/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage : string

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
        Description : string

        /// AppSettings parameter separator
        AppSettingsSeparators : string []
        /// AppSettings parameter split options
        AppSettingsSplitOptions : StringSplitOptions

        /// If specified, should consume remaining tokens from the CLI
        IsRest : bool
        /// If specified, parameter can only be at start of CLI parameters
        IsFirst : bool
        /// If specified, use '--param=arg' CLI parsing syntax
        IsEquals1Assignment : bool
        /// If specified, use '--param key=value' CLI parsing syntax
        IsEquals2Assignment : bool
        /// If specified, multiple parameters can be added in AppSettings in CSV form.
        AppSettingsCSV : bool
        /// Fails if no argument of this type is specified
        IsMandatory : bool
        /// Specifies that argument should be specified at most once in CLI
        IsUnique : bool
        /// Hide from Usage
        IsHidden : bool
        /// Combine AppSettings with CLI inputs
        GatherAllSources : bool
    }