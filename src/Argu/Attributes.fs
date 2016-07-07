/// Argu Attribute declaration module
[<AutoOpen>]
module Argu.ArguAttributes

open System

/// Parse multiple parameters in AppSettings as comma separated values. OBSOLETE
[<Obsolete("Please use list parameters instead.")>]
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type ParseCSVAttribute () = inherit Attribute ()

/// Consume all remaining CLI tokens using this parameter wherever it might occur. OBSOLETE
[<Obsolete("Please use list parameters instead.")>]
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type RestAttribute () = inherit Attribute ()

/// Hides argument from command line argument usage string.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type HiddenAttribute () = inherit Attribute ()

/// Demands at least one parsed result for this argument; a parse exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type MandatoryAttribute () = inherit Attribute ()

/// Demands that the argument should be specified at most once; a parse exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type UniqueAttribute () = inherit Attribute ()

/// Demands that the argument should be specified exactly once; a parse exception is raised otherwise.
/// Equivalent to attaching both the Mandatory and Unique attribute on the parameter.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type ExactlyOnceAttribute () = inherit Attribute ()

/// Denotes that the given argument should be inherited in the scope of any subcommands.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type InheritAttribute() = inherit Attribute()

/// Denotes that the given argument should accummulate any unrecognized arguments it encounters.
/// Must contain a single field of type string
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type GatherUnrecognized() = inherit Attribute()

/// Demands that at least one subcommand is specified in the CLI; a parse exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type RequireSubcommandAttribute () = inherit Attribute()

/// Requires that CLI parameters should not override AppSettings parameters.
/// Will return parsed results from both AppSettings and CLI.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type GatherAllSourcesAttribute () = inherit Attribute ()

/// Disable CLI parsing for this argument. Use for AppSettings parsing only.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type NoCommandLineAttribute () = inherit Attribute ()

/// Disable AppSettings parsing for this branch. Use for CLI parsing only.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type NoAppSettingsAttribute () = inherit Attribute ()

/// Specifies a custom set of Help/Usage switches for the CLI.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type HelpFlagsAttribute ([<ParamArray>] names : string []) = 
    inherit Attribute()
    member __.Names = names

/// Specifies that Help/Usage switches should be disabled for the CLI.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type DisableHelpFlagsAttribute () = inherit HelpFlagsAttribute ()

/// Specifies a custom description text for the Help/Usage switches in the usage string.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type HelpDescriptionAttribute (description : string) =
    inherit Attribute()
    member __.Description = description

/// Declares that argument should be placed at specific position.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type CliPositionAttribute(position : CliPosition) =
    inherit Attribute()
    member __.Position = position

/// Declares that argument can only be placed at the beginning of the CLI syntax.
/// A parse exception will be raised if that is not the case.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type FirstAttribute () = inherit CliPositionAttribute (CliPosition.First)

/// Declares that argument can only be placed at the end of the CLI syntax.
/// A parse exception will be raised if that is not the case.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type LastAttribute () = inherit CliPositionAttribute (CliPosition.Last)

/// Declares that argument is the main command of the CLI syntax.
/// Arguments are specified without requiring a switch.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type MainCommandAttribute (argumentName : string) = 
    inherit Attribute()
    new () = new MainCommandAttribute(null)
    member __.ArgumentName = argumentName

/// Print F# 3.1 field labels in usage string. OBSOLETE
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Argu 3.0 prints union labels by default. Please remove this attribute.")>]
type PrintLabelsAttribute () = inherit Attribute ()

/// Use '--param=arg' or '--param key=value' assignment syntax in CLI.
/// Requires that the argument should have parameters of arity 1 or 2 only.
/// Can be used to specify any assignment separator.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type CustomAssignmentAttribute (separator : string) = 
    inherit Attribute ()
    member __.Separator = separator

/// Use '--param=arg' or '--param key=value' assignment syntax in CLI.
/// Requires that the argument should have parameters of arity 1 or 2 only.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type EqualsAssignmentAttribute () = 
    inherit CustomAssignmentAttribute("=")

/// Use '--param:arg' or '--param key:value' assignment syntax in CLI.
/// Requires that the argument should have parameters of arity 1 or 2 only.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type ColonAssignmentAttribute () = 
    inherit CustomAssignmentAttribute(":")

/// Declares a custom default CLI identifier for the current parameter.
/// Replaces the auto-generated identifier name.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type CustomCommandLineAttribute (name : string, [<ParamArray>]altNames : string []) =
    inherit Attribute ()
    member __.Name = name
    member __.AltNames = altNames

/// Declares a set of secondary CLI identifiers for the current parameter.
/// Does not replace the default identifier which is either auto-generated
/// or specified by the CustomCommandLine attribute.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type AltCommandLineAttribute ([<ParamArray>] names : string []) = 
    inherit Attribute ()
    member __.Names = names

/// Declares a custom key identifier for the current parameter in AppSettings parsing.
/// Replaces the auto-generated identifier name.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type CustomAppSettingsAttribute (name : string) =
    inherit Attribute ()
    member __.Name = name

/// Specify a custom value separator in AppSettings parsing parameters.
/// Used in CSV or list-based parameter parsing.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type AppSettingsSeparatorAttribute ([<ParamArray>] separators : string [], splitOptions : StringSplitOptions) =
    inherit Attribute()
    new (separator : char) = new AppSettingsSeparatorAttribute([|string separator|], StringSplitOptions.None)
    member __.Separators = separators
    member __.SplitOptions = splitOptions

/// Specifies a custom prefix for auto-generated CLI names.
/// This defaults to double dash ('--').
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Class, AllowMultiple = false)>]
type CliPrefixAttribute(prefix : string) = 
    inherit Attribute() 
    member __.Prefix = prefix