[<AutoOpen>]
module internal Argu.UnionArgInfo

open System
open System.IO
open System.Configuration
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization.Formatters.Binary
open System.Text.RegularExpressions

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns

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
        /// parser
        Parser : string -> obj
        /// unparser
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
        /// UCI identifier
        UnionCaseInfo : UnionCaseInfo
        /// Field parser definitions or nested union argument
        FieldParsers : ParameterType

        /// Gets the parent record for union case
        GetParent : unit -> UnionArgInfo

        /// Builds a union case out of its field parameters
        CaseCtor : obj [] -> obj
        /// Composes case fields into a tuple, if not nullary
        FieldCtor : Lazy<(obj [] -> obj) option>
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
        /// If specified, use '--param=arg' CLI parsing syntax
        IsEquals1Assignment : bool
        /// If specified, use '--param key=value' CLI parsing syntax
        IsEquals2Assignment : bool
        /// If specified, multiple parameters can be added in Configuration in CSV form.
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
with
    member inline __.Tag = __.UnionCaseInfo.Tag
    member inline __.NoCommandLine = __.CommandLineNames.Length = 0
    member inline __.Type =
        match __.FieldParsers with
        | Primitives _ -> ArgumentType.Primitive
        | OptionalParam _ -> ArgumentType.Optional
        | ListParam _ -> ArgumentType.List
        | NestedUnion _ -> ArgumentType.SubCommand

and ParameterType =
    | Primitives of FieldParserInfo []
    | OptionalParam of Existential * FieldParserInfo
    | ListParam of Existential * FieldParserInfo
    | NestedUnion of ShapeArgumentTemplate * UnionArgInfo
with
    member inline t.IsNested = match t with NestedUnion _ -> true | _ -> false

and [<NoEquality; NoComparison>] 
  UnionArgInfo =
    {
        /// Union Case Argument Info
        Type : Type
        /// If subcommand, attempt to retrieve the parent record
        TryGetParent : unit -> UnionCaseArgInfo option
        /// Union cases
        Cases : UnionCaseArgInfo []
        /// Precomputed union tag reader
        TagReader : Lazy<obj -> int>
        /// Single character switches
        GroupedSwitchExtractor : Lazy<string -> string []>
        /// Union cases indexed by appsettings parameter names
        AppSettingsParamIndex : Lazy<IDictionary<string, UnionCaseArgInfo>>
        /// Help flags specified by the library
        HelpParam : HelpParam
        /// Union cases indexed by cli parameter names
        CliParamIndex : Lazy<IDictionary<string, UnionCaseArgInfo>>
    }
with
    member inline uai.UsesHelpParam = List.isEmpty uai.HelpParam.Flags |> not


[<NoEquality; NoComparison>]
type UnionCaseParseResult =
    {
        /// union case value
        Value : obj
        /// untyped version of tuple of branch contents
        FieldContents : obj
        /// Index denoting order of parse result
        Index : int
        /// ArgInfo used to parse parameter
        ArgInfo : UnionCaseArgInfo
        /// metadata provided by the parser
        ParseContext : string
        /// parse source 
        Source : ParseSource
    }
with
    member inline __.Tag = __.ArgInfo.Tag

[<NoEquality; NoComparison>]
type UnionParseResults =
    {
        /// Results by union case
        Cases : UnionCaseParseResult[][]
        /// CLI tokens not recognized by the parser
        UnrecognizedCliParams : string list
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
            IsEquals1Assignment = ucai.IsEquals1Assignment
            IsEquals2Assignment = ucai.IsEquals2Assignment
            AppSettingsCSV = ucai.AppSettingsCSV
            IsMandatory = ucai.IsMandatory
            IsUnique = ucai.IsUnique
            IsHidden = ucai.IsHidden
            GatherAllSources = ucai.GatherAllSources
        }