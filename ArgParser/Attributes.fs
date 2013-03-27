namespace UnionArgParser

    // interfaces used by the library

    type IArgParserTemplate =
        abstract Usage : string

    and Exiter =
        abstract Exit : ?msg : string * ?id : int -> 'T


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

    /// Sets a custom command line name.
    type CustomCommandLineAttribute (name : string) =
        inherit Attribute ()
        member __.Name = name

    /// Sets alternative command line names.
    type AltCommandLineAttribute (name : string) = 
        inherit Attribute ()
        member __.Name = name

    /// Sets a custom AppSettings key name.
    type CustomAppSettingsAttribute (name : string) = 
        inherit Attribute ()
        member __.Name = name