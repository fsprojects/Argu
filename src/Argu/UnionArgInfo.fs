﻿[<AutoOpen>]
module internal Argu.UnionArgInfo

open System
open System.IO
open System.Configuration
open System.Collections.Generic
open System.Reflection

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns

type IParseResult =
    abstract GetAllResults : unit -> seq<obj> 

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type CustomAssignmentResult =
    | NoAssignment
    | Assignment of parameter:string * separator:string * value:string

/// Union Case Field info
[<NoEquality; NoComparison>]
type FieldParserInfo =
    {
        /// Type name
        Name : string
        /// field label
        Label : string option
        /// field type
        Type : Type
        /// string to field value parser
        Parser : string -> obj
        /// field value to string unparser
        UnParser : obj -> string
    }
with
    member inline p.Description =
        match p.Label with
        | None -> p.Name
        | Some l -> l

/// Help Param description
type HelpParam =
    {
        Flags : string list
        Description : string
    }
with
    member inline hp.IsHelpFlag(flag : string) =
        let rec aux = function 
            | [] -> false 
            | h :: tl' -> if h = flag then true else aux tl'

        aux hp.Flags
        
/// Represents a parsing schema for a single parameter
[<NoEquality; NoComparison>]
type UnionCaseArgInfo =
    {
        /// Human readable name identifier
        Name : string
        /// Contextual depth of current argument w.r.t subcommands
        Depth : int
        /// Numbers of parameters in the given union case
        Arity : int
        /// UCI identifier
        UnionCaseInfo : UnionCaseInfo
        /// Field parser definitions or nested union argument
        ParameterInfo : ParameterInfo

        /// Gets the parent record for union case
        GetParent : unit -> UnionArgInfo

        /// Builds a union case out of its field parameters
        CaseCtor : obj [] -> obj
        /// Composes case fields into a parametric tuple, if not nullary
        FieldCtor : Lazy<obj [] -> obj>
        /// Decomposes a case instance into an array of fields
        FieldReader : Lazy<obj -> obj[]>

        /// head element denotes primary command line arg
        CommandLineNames : string list
        /// name used in AppSettings
        AppSettingsName : string option

        /// Description of the parameter
        Description : string

        /// Configuration parsing parameter separator
        AppSettingsSeparators : string []
        /// Configuration parsing split options
        AppSettingsSplitOptions : StringSplitOptions

        /// If specified, should consume remaining tokens from the CLI
        IsRest : bool
        /// If specified, parameter can only be at start of CLI parameters
        IsFirst : bool
        /// Separator token used for EqualsAssignment syntax; e.g. '=' forces '--param=arg' syntax
        CustomAssignmentSeparator : string option
        /// If specified, multiple parameters can be added in Configuration in CSV form.
        AppSettingsCSV : bool
        /// Fails if no argument of this type is specified
        IsMandatory : bool
        /// Indicates that argument should be inherited in the scope of any sibling subcommands.
        IsInherited : bool
        /// Specifies that argument should be specified at most once in CLI
        IsUnique : bool
        /// Hide from Usage
        IsHidden : bool
        /// Combine AppSettings with CLI inputs
        GatherAllSources : bool
    }
with
    member inline __.Tag = __.UnionCaseInfo.Tag
    member inline __.NoCommandLine = __.CommandLineNames.Length = 0
    member inline __.Type = __.ParameterInfo.Type
    member inline __.IsCustomAssignment = Option.isSome __.CustomAssignmentSeparator
    member inline __.IsMatchingAssignmentSeparator (separator : string) =
        match __.CustomAssignmentSeparator with
        | Some sep -> sep = separator
        | None -> false
    member inline x.IsMandatorySpecialFirst =
      x.IsFirst && x.CommandLineNames |> Seq.exists ((=) "") && x.IsMandatory

and ParameterInfo =
    | Primitives of FieldParserInfo []
    | OptionalParam of Existential * FieldParserInfo
    | ListParam of Existential * FieldParserInfo
    | SubCommand of ShapeArgumentTemplate * argInfo:UnionArgInfo * label:string option
with
    member pI.Type =
        match pI with
        | Primitives _ -> ArgumentType.Primitive
        | OptionalParam _ -> ArgumentType.Optional
        | ListParam _ -> ArgumentType.List
        | SubCommand _ -> ArgumentType.SubCommand

and [<NoEquality; NoComparison>] 
  UnionArgInfo =
    {
        /// Union Case Argument Info
        Type : Type
        /// Contextual depth of current argument w.r.t subcommands
        Depth : int
        /// If subcommand, attempt to retrieve the parent record
        TryGetParent : unit -> UnionCaseArgInfo option
        /// Union cases
        Cases : UnionCaseArgInfo []
        /// Help flags specified by the library
        HelpParam : HelpParam
        /// Denotes that the current argument contains subcommands
        ContainsSubcommands : bool
        /// Specifies that CLI parse results require a subcommand
        IsRequiredSubcommand : bool
        /// Precomputed union tag reader
        TagReader : Lazy<obj -> int>
        /// Arguments inherited by parent commands
        InheritedParams : Lazy<UnionCaseArgInfo []>
        /// Assignment recognizer
        AssignmentRecognizer : Lazy<string -> CustomAssignmentResult>
        /// Single character switches
        GroupedSwitchExtractor : Lazy<string -> string []>
        /// Union cases indexed by appsettings parameter names
        AppSettingsParamIndex : Lazy<IDictionary<string, UnionCaseArgInfo>>
        /// Union cases indexed by cli parameter names
        CliParamIndex : Lazy<IDictionary<string, UnionCaseArgInfo>>
    }
with
    member inline uai.UsesHelpParam = List.isEmpty uai.HelpParam.Flags |> not


[<NoEquality; NoComparison>]
type UnionCaseParseResult =
    {
        /// Parsed field parameters
        Fields : obj[]
        /// Index denoting order of parse result
        Index : int
        /// ArgInfo used to parse parameter
        CaseInfo : UnionCaseArgInfo
        /// metadata provided by the parser
        ParseContext : string
        /// parse source 
        Source : ParseSource
    }
with
    member inline __.Tag = __.CaseInfo.Tag
    member inline __.Value = __.CaseInfo.CaseCtor __.Fields
    member inline __.FieldContents = __.CaseInfo.FieldCtor.Value __.Fields

[<NoEquality; NoComparison>]
type UnionParseResults =
    {
        /// Results by union case
        Cases : UnionCaseParseResult[][]
        /// CLI tokens not recognized by the parser
        UnrecognizedCliParams : string list
        /// CLI parse objects not belonging to the current parser context
        UnrecognizedCliParseResults : obj list
        /// Usage string requested by the caller
        IsUsageRequested : bool
    }

type UnionCaseArgInfo with
    member ucai.ToArgumentCaseInfo() : ArgumentCaseInfo =
        {
            Name = ucai.Name
            ArgumentType = ucai.Type
            UnionCaseInfo = ucai.UnionCaseInfo
            CommandLineNames = ucai.CommandLineNames
            AppSettingsName = ucai.AppSettingsName
            Description = ucai.Description
            AppSettingsSeparators = ucai.AppSettingsSeparators
            AppSettingsSplitOptions = ucai.AppSettingsSplitOptions
            IsRest = ucai.IsRest
            IsFirst = ucai.IsFirst
            CustomAssignmentSeparator = ucai.CustomAssignmentSeparator
            AppSettingsCSV = ucai.AppSettingsCSV
            IsMandatory = ucai.IsMandatory
            IsUnique = ucai.IsUnique
            IsHidden = ucai.IsHidden
            GatherAllSources = ucai.GatherAllSources
        }