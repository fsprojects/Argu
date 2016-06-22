namespace Argu

open System

// Attribute declarations

/// Parse comma separated values in AppSettings
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type ParseCSVAttribute () = inherit Attribute ()

/// Consume all remaining command line arguments.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type RestAttribute () = inherit Attribute ()

/// Hide from command line argument documentation.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type HiddenAttribute () = inherit Attribute ()

/// Demands at least one parsed result for this branch; an exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type MandatoryAttribute () = inherit Attribute ()

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

/// Print F# 3.1 field labels in 'Usage' string.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type PrintLabelsAttribute () = inherit Attribute ()

/// Use '--param=arg' assignment syntax in CLI.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
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

/// Sets a custom AppSettings key name.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type CustomAppSettingsAttribute (name : string) = 
    inherit Attribute ()
    member __.Name = name

/// Predefined CLI prefixes to be added
/// 
[<RequireQualifiedAccess>]
module CliPrefix =
    /// No Cli Prefix
    let [<Literal>] None = ""
    /// Single Dash prefix '-'
    let [<Literal>] Dash = "-"
    /// Double Dash prefix '--'
    let [<Literal>] DoubleDash = "--"

/// Specifies a custom prefix for auto generated CLI names.
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Class, AllowMultiple = false)>]
type CliPrefixAttribute(prefix : string) = 
    inherit Attribute() 
    member __.Prefix = prefix

/// Source from which to parse arguments
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

/// An interface for error handling in the argument parser
type IExiter =
    abstract Exit : msg : string * ?errorCode : int -> 'T

/// Handles argument parser errors by raising an exception
and ExceptionExiter(ctor : string -> exn) =
    static member ArgumentExceptionExiter () = 
        new ExceptionExiter(fun msg -> new System.ArgumentException(msg) :> _) :> IExiter
    interface IExiter with
        member __.Exit(msg, _) = raise (ctor msg)

/// Handles argument parser errors by exiting the process
/// after printing a parse error.
and ProcessExiter() =
    interface IExiter with
        member __.Exit(msg : string, ?errorCode) =
            Console.Error.WriteLine msg
            do Console.Error.Flush()
            Microsoft.FSharp.Core.Operators.exit (defaultArg errorCode 1)