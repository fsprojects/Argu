namespace Nessos.UnionArgParser

    type ParseSource = AppSettings | CommandLine

    /// Interface that must be implemented by all UnionArgParser template types
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
                System.Console.Error.WriteLine msg
                do System.Console.Error.Flush()
                Microsoft.FSharp.Core.Operators.exit (defaultArg errorCode 1)



    // Attribute declarations

    open System

    /// Parse comma separated values in AppSettings
    type ParseCSVAttribute () = inherit Attribute ()
    /// Consume all remaining command line arguments.
    type RestAttribute () = inherit Attribute ()
    /// Hide from command line argument documentation.
    type HiddenAttribute () = inherit Attribute ()
    /// Demands at least one parsed result for this branch; an exception is raised otherwise.
    type MandatoryAttribute () = inherit Attribute ()
    /// Gathers all parsed results from both AppSettings and command line.
    type GatherAllSourcesAttribute () = inherit Attribute ()
    /// Disable command line parsing for this branch.
    type NoCommandLineAttribute () = inherit Attribute ()
    /// Disable AppSettings parsing for this branch.
    type NoAppSettingsAttribute () = inherit Attribute ()
    /// Argument can only be placed at the beginning of the command line.
    type FirstAttribute () = inherit Attribute ()
    /// Print F# 3.1 field labels in 'Usage' string.
    type PrintLabelsAttribute () = inherit Attribute ()
    /// Use Base64 encoding to pass binary parameters.
    type EncodeBase64Attribute () = inherit Attribute ()
    /// Use '--param=arg' assignment syntax in CLI.
    type EqualsAssignmentAttribute () = inherit Attribute ()

    /// Sets a custom command line name.
    type CustomCommandLineAttribute (name : string) =
        inherit Attribute ()
        member __.Name = name

    /// Sets alternative command line names.
    [<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
    type AltCommandLineAttribute (name : string) = 
        inherit Attribute ()
        member __.Name = name

    /// Sets a custom AppSettings key name.
    type CustomAppSettingsAttribute (name : string) = 
        inherit Attribute ()
        member __.Name = name