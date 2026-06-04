/// Argu Attribute declaration module
[<AutoOpen>]
module Argu.ArguAttributes

// Several attributes in this file are deprecated but kept as functional
// wrappers for source-compat (e.g. EqualsAssignmentAttribute inherits from
// the now-obsolete CustomAssignmentAttribute). #nowarn 44 silences the
// resulting FS0044 inside this file; callers still see the warning.
#nowarn "44"

open System

/// Parse multiple parameters in AppSettings as comma separated values. OBSOLETE
[<Obsolete("Please use list parameters instead.")>]
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type ParseCSVAttribute () = inherit Attribute ()

/// Consume all remaining CLI tokens using this parameter wherever it might occur. OBSOLETE
[<Obsolete("Please use list parameters instead.")>]
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type RestAttribute () = inherit Attribute ()

/// Hides argument from command line argument usage string.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type HiddenAttribute () = inherit Attribute ()

/// Demands at least one parsed result for this argument; a parse exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type MandatoryAttribute () = inherit Attribute ()

/// Demands that the argument should be specified at most once; a parse exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type UniqueAttribute () = inherit Attribute ()

/// Demands that the argument should be specified exactly once; a parse exception is raised otherwise.
/// Equivalent to attaching both the Mandatory and Unique attribute on the parameter.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type ExactlyOnceAttribute () = inherit Attribute ()

/// Denotes that the given argument should be inherited in the scope of any subcommands.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type InheritAttribute() = inherit Attribute()

/// Denotes that the given argument should accumulate any unrecognized arguments it encounters.
/// Must contain a single field of type string
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type GatherUnrecognizedAttribute() = inherit Attribute()

/// Demands that at least one subcommand is specified in the CLI; a parse exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type RequireSubcommandAttribute () = inherit Attribute()

/// Requires that CLI parameters should not override AppSettings parameters.
/// Will return parsed results from both AppSettings and CLI.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type GatherAllSourcesAttribute () = inherit Attribute ()

/// Disable CLI parsing for this argument. Use for AppSettings parsing only.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type NoCommandLineAttribute () = inherit Attribute ()

/// Disable AppSettings parsing for this branch. Use for CLI parsing only.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type NoAppSettingsAttribute () = inherit Attribute ()

/// Specifies a custom set of Help/Usage switches for the CLI.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type HelpFlagsAttribute ([<ParamArray>] names : string []) =
    inherit Attribute()
    /// CLI switches that trigger help text rendering.
    member _.Names = names

/// Specifies that Help/Usage switches should be disabled for the CLI.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type DisableHelpFlagsAttribute () = inherit HelpFlagsAttribute ()

/// Specifies a custom description text for the Help/Usage switches in the usage string.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type HelpDescriptionAttribute (description : string) =
    inherit Attribute()
    /// The custom description rendered next to the help switches in the usage string.
    member _.Description = description

/// Declares that argument should be placed at specific position.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type CliPositionAttribute(position : CliPosition) =
    inherit Attribute()
    /// The mandated CLI position (First, Last, or Unspecified).
    member _.Position = position

/// Declares that argument can only be placed at the beginning of the CLI syntax.
/// A parse exception will be raised if that is not the case.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type FirstAttribute () = inherit CliPositionAttribute (CliPosition.First)

/// Declares that argument can only be placed at the end of the CLI syntax.
/// A parse exception will be raised if that is not the case.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type LastAttribute () = inherit CliPositionAttribute (CliPosition.Last)

/// Declares that argument is a subcommand.
/// A parse exception will be raised if the argument has parameters
/// and their type is not ParseResults<_>.
/// Implicit if the argument does have a parameter of type ParseResults<_>.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type SubCommandAttribute () = inherit Attribute()

/// Declares that argument is the main command of the CLI syntax.
/// Arguments are specified without requiring a switch.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type MainCommandAttribute (argumentName : string) =
    inherit Attribute()
    new () = MainCommandAttribute(null)
    /// Optional label used in the rendered usage string. <c>null</c> means use the auto-generated name.
    member _.ArgumentName = argumentName

/// Print F# 3.1 field labels in usage string. OBSOLETE
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Argu 3.0 prints union labels by default. Please remove this attribute.")>]
type PrintLabelsAttribute () = inherit Attribute ()

/// <summary>
/// Use a custom separator for parameter assignment.<br/>
/// When <paramref name="allowSpaced"/> is <c>true</c>, the format <c>--param value</c> will also be accepted.<br/>
/// Otherwise <c>'--param&lt;separator&gt;arg'</c> or <c>'--param key&lt;separator&gt;value'</c> are accepted.<br/>
/// </summary>
/// <remarks>
/// Unified attribute that subsumes the six legacy predecessors: <c>CustomAssignment</c>,
/// <c>EqualsAssignment</c>, <c>ColonAssignment</c> and the <c>*OrSpaced</c> attributes.<br/>
/// </remarks>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type AssignmentAttribute (separator : string, allowSpaced : bool) =
    inherit Attribute ()
    new (separator : string) = AssignmentAttribute(separator, false)
    /// The assignment separator string (e.g. "=" or ":").
    member _.Separator = separator
    /// When <c>true</c>, the spaced form (<c>--param value</c>) is also accepted alongside the separator form (only supported for non-keyed parameters).
    member _.AllowSpaced = allowSpaced

/// Use a custom separator for parameter assignment.
/// e.g. '--param<separator>arg' or '--param key<separator>value'.
/// Requires that the argument should have parameters of arity 1 or 2 only.
/// Can be used to specify any assignment separator.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Use [<Assignment(separator)>] instead.")>]
type CustomAssignmentAttribute (separator : string) =
    inherit Attribute ()
    /// The assignment separator string (e.g. "=" or ":").
    member _.Separator = separator

/// Use '--param=arg' or '--param key=value' assignment syntax in CLI.
/// Requires that the argument should have parameters of arity 1 or 2 only.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Use [<Assignment(\"=\")>] instead.")>]
type EqualsAssignmentAttribute () =
    inherit CustomAssignmentAttribute("=")

/// Use '--param:arg' or '--param key:value' assignment syntax in CLI.
/// Requires that the argument should have parameters of arity 1 or 2 only.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Use [<Assignment(\":\")>] instead.")>]
type ColonAssignmentAttribute () =
    inherit CustomAssignmentAttribute(":")

/// Use a custom separator for parameter assignment.
/// e.g. '--param<separator>arg'
/// Parameters can also be assigned using space as separator e.g. '--param arg'
/// Requires that the argument should have parameters of arity 1 only.
/// Can be used to specify any assignment separator.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Use [<Assignment(separator, allowSpaced = true)>] instead.")>]
type CustomAssignmentOrSpacedAttribute (separator : string) =
    inherit Attribute ()
    /// The assignment separator string (e.g. "=" or ":"). Spaced form is also accepted.
    member _.Separator = separator

/// Use '--param=arg' assignment syntax in CLI.
/// Parameters can also be assigned using space as separator e.g. '--param arg'
/// Requires that the argument should have parameters of arity 1 only.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Use [<Assignment(\"=\", allowSpaced = true)>] instead.")>]
type EqualsAssignmentOrSpacedAttribute () =
    inherit CustomAssignmentOrSpacedAttribute("=")

/// Use '--param:arg' assignment syntax in CLI.
/// Parameters can also be assigned using space as separator e.g. '--param arg'
/// Requires that the argument should have parameters of arity 1 only.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<Obsolete("Use [<Assignment(\":\", allowSpaced = true)>] instead.")>]
type ColonAssignmentOrSpacedAttribute () =
    inherit CustomAssignmentOrSpacedAttribute(":")

/// Declares a custom default CLI identifier for the current parameter.
/// Replaces the auto-generated identifier name.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type CustomCommandLineAttribute (name : string, [<ParamArray>]altNames : string []) =
    inherit Attribute ()
    /// The primary CLI identifier, replacing the auto-generated name.
    member _.Name = name
    /// Additional alias names accepted on the CLI alongside <see cref="Name"/>.
    member _.AltNames = altNames

/// Declares a set of secondary CLI identifiers for the current parameter.
/// Does not replace the default identifier which is either auto-generated
/// or specified by the CustomCommandLine attribute.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = true)>]
type AltCommandLineAttribute ([<ParamArray>] names : string []) =
    inherit Attribute ()
    /// Secondary CLI identifiers accepted alongside the primary name.
    member _.Names = names

/// Declares a custom key identifier for the current parameter in AppSettings parsing.
/// Replaces the auto-generated identifier name.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type CustomAppSettingsAttribute (name : string) =
    inherit Attribute ()
    /// The AppSettings key identifier, replacing the auto-generated name.
    member _.Name = name

/// Specify a custom value separator in AppSettings parsing parameters.
/// Used in CSV or list-based parameter parsing.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type AppSettingsSeparatorAttribute ([<ParamArray>] separators : string [], splitOptions : StringSplitOptions) =
    inherit Attribute()
    new (separator : char) = AppSettingsSeparatorAttribute([|string separator|], StringSplitOptions.None)
    /// The separator strings used to split AppSettings list/CSV values.
    member _.Separators = separators
    /// Split options applied during AppSettings list/CSV value separation.
    member _.SplitOptions = splitOptions

/// Specifies a custom prefix for auto-generated CLI names.
/// This defaults to double dash ('--').
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Class, AllowMultiple = false)>]
type CliPrefixAttribute(prefix : string) =
    inherit Attribute()
    /// The auto-generation prefix (e.g. "--", "-", or empty).
    member _.Prefix = prefix
