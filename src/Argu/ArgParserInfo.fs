[<AutoOpen>]
module internal Argu.ArgParserInfo

open Microsoft.FSharp.Reflection

/// Represents a parsing schema for a single parameter
[<NoEquality; NoComparison>]
type ArgCaseInfo<'Template> =
    {
        /// Union Case Info identifier for argument
        UnionCaseInfo : UnionCaseInfo

        /// Field parser definitions
        FieldParsers : ParserInfo []

        /// Builds a union case out of its field parameters
        CaseCtor : obj [] -> obj
        /// Composes case fields into a tuple, if not nullary
        FieldCtor : (obj [] -> obj) option

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
    member __.NoCommandLine = __.CommandLineNames.IsEmpty

//[<NoEquality; NoComparison>]
//type ParseResult<'T> =
//    {
//        /// union case value
//        Value : 'T
//
//        /// Tupled form case parameters, if any
//        FieldContents : obj
//            
//        /// ArgInfo used to parse parameter
//        UnionCaseInfo : UnionCaseInfo
//
//        /// metadata provided by the parser
//        ParseContext : string
//            
//        /// parse source 
//        Source : ParseSource
//    }
//
//type internal ParseResults<'T> =
//    {
//        Results : ParseResult<'T>
//        IsUsageRequested : bool
//    }