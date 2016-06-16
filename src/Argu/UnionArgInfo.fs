[<AutoOpen>]
module internal Argu.UnionArgInfo

open System
open System.IO
open System.Configuration
open System.Reflection
open System.Runtime.Serialization.Formatters.Binary
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type ErrorCode =
    | HelpText = 0
    | AppSettings = 2
    | CommandLine = 3
    | PostProcess = 4

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

        /// Builds a union case out of its field parameters
        CaseCtor : obj [] -> obj
        /// Composes case fields into a tuple, if not nullary
        FieldCtor : (obj [] -> obj) option
        /// Decomposes a case instance into an array of fields
        FieldReader : obj -> obj[]

        /// head element denotes primary command line arg
        CommandLineNames : string list
        /// name used in AppSettings
        AppSettingsName : string option

        /// Description of the parameter
        Usage : string

        /// If specified, should consume remaining tokens from the CLI
        IsRest : bool
        /// If specified, parameter can only be at start of CLI parameters
        IsFirst : bool
        /// If specified, use '--param=arg' CLI parsing syntax
        IsEqualsAssignment : bool
        // Declares that the given argument should replace the default '--help' parameter template
        IsHelpParameter : bool
        /// Print labels in Usage ()
        PrintLabels : bool
        /// If specified, multiple parameters can be added in AppSettings in CSV form.
        AppSettingsCSV : bool
        /// Fails if no argument of this type is specified
        Mandatory : bool
        /// Hide from Usage
        Hidden : bool
        /// Combine AppSettings with CLI inputs
        GatherAllSources : bool
    }
with
    member inline __.Tag = __.UnionCaseInfo.Tag
    member inline __.NoCommandLine = __.CommandLineNames.Length = 0
    member inline __.IsNested = match __.FieldParsers with NestedUnion _ -> true | _ -> false

and ParameterType =
    | Primitives of FieldParserInfo []
    | NestedUnion of UnionArgInfo

and [<NoEquality; NoComparison>] 
  UnionArgInfo =
    {
        /// Union Case Argument Info
        Type : Type
        /// Parents
        Parents : string []
        /// Union cases
        Cases : UnionCaseArgInfo []
        /// Precomputed union tag reader
        TagReader : obj -> int
        /// Default help CLI flags
        UseDefaultHelper : UnionCaseArgInfo option
        /// Constructs a usage string for given argument
        MakeUsageString : string option -> string
        /// Single character switches
        CharacterSwitches : Regex
    }


[<NoEquality; NoComparison>]
type UnionCaseParseResult =
    {
        /// union case value
        Value : obj
        /// untyped version of tuple of branch contents
        FieldContents : obj
        /// ArgInfo used to parse parameter
        ArgInfo : UnionCaseArgInfo
        /// metadata provided by the parser
        ParseContext : string
        /// parse source 
        Source : ParseSource
    }

[<NoEquality; NoComparison>]
type UnionParseResults =
    {
        /// Results by union case
        Cases : UnionCaseParseResult[][]
        /// Usage string requested by the caller
        IsUsageRequested : bool
    }
    

//with
//    override p.ToString() =
//        match p.Label with
//        | None -> p.Name
//        | Some l -> sprintf "%s:%s" l p.Name