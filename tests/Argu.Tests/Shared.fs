namespace Argu.Tests

#nowarn "44"

open System
open Argu

/// Shared union types and parser instance used by the bulk of the test suite.
/// The original 1100-line Tests.fs has been split so this base schema lives in
/// one place; individual test files `open Argu.Tests.Shared` to pull it in.
module Shared =

    type Exception with
        member inline x.FirstLine =
            x.Message.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries).[0]

    [<Flags>]
    type Enum =
        | First     = 1
        | Second    = 2
        | Third     = 3

    type Enumeration =
        | First
        | Second
        | Third

    type PushArgs =
        | [<AltCommandLine("-f")>] Force
        | [<MainCommand("COMMAND"); ExactlyOnce>] Remote of repo_name:string * branch_name:string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Force -> "force changes in remote repo"
                | Remote _ -> "push changes to remote repository and branch"

    type NewArgs =
        | [<Mandatory>] Name of string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Name _ -> "New name"

    type TagArgs =
        | New of ParseResults<NewArgs>
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | New _ -> "New tag"

    type CheckoutArgs =
        | [<Mandatory>] Branch of string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Branch _ -> "push changes to remote repository and branch"

    [<CliPrefix(CliPrefix.Dash)>]
    type CleanArgs =
        | D
        | F
        | X
        interface IArgParserTemplate with
            member this.Usage = "clean"

    [<RequireSubcommand>]
    type RequiredSubcommand =
        | Foo
        | [<CliPrefix(CliPrefix.None)>] Sub of ParseResults<CleanArgs>
        | [<SubCommand; CliPrefix(CliPrefix.None)>] Null_Sub
        interface IArgParserTemplate with
            member this.Usage = "required"

    type GatherUnrecognizedSubcommand =
        | Switch1
        | Switch2
        | [<GatherUnrecognized; Hidden>] Unrec of values:string
        interface IArgParserTemplate with
            member this.Usage = "gus"

    type MultipleMandatoriesSubCommand =
        | [<Mandatory>] ValueA of int
        | [<Mandatory>] ValueB of int
        | [<Mandatory>] ValueC of int
        | ValueD of int
        interface IArgParserTemplate with member this.Usage = "multiple mandatories subcommand arg"

    type Argument =
        | [<AltCommandLine("-v"); Inherit>] Verbose
        | Working_Directory of string
        | [<AppSettingsSeparator(':')>] Listener of host:string * port:int
        | [<Mandatory>] Mandatory_Arg of bool
        | [<Unique>] Unique_Arg of bool
        | [<Rest; ParseCSV>] Rest_Arg of int
        | [<MainCommand; Last; Unique>] Main of chars:char list
        | [<Inherit>] Data of int * byte []
        | Log_Level of int
        | Float32_Arg of float32
        | Float64_Arg of float
        | Decimal_Arg of decimal
        | [<AltCommandLine("/D", "-D", "-z")>] Detach
        | [<CustomAppSettings "Foo">] CustomAppConfig of string * int
        | [<ColonAssignment>] Assignment of string
        | [<EqualsAssignment>] Env of key:string * value:string
        | [<EqualsAssignment>] Dir of path:string
        | [<EqualsAssignmentOrSpaced>] Flex_Equals_Assignment of string
        | [<EqualsAssignmentOrSpaced>] Flex_Equals_Assignment_With_Option of string option
        | [<ColonAssignmentOrSpaced>] Flex_Colon_Assignment of string
        | [<First>] First_Parameter of string
        | [<Last>] Last_Parameter of string
        | Optional of int option
        | List of int list
        | Enum of Enum
        | [<EqualsAssignment>] Enumeration of Enumeration option
        | [<CliPrefix(CliPrefix.Dash)>] A
        | [<CliPrefix(CliPrefix.Dash)>] B
        | [<CliPrefix(CliPrefix.Dash)>] C
        | [<CliPrefix(CliPrefix.None)>] Push of ParseResults<PushArgs>
        | [<CliPrefix(CliPrefix.None)>] Checkout of ParseResults<CheckoutArgs>
        | [<CliPrefix(CliPrefix.None)>] Tag of ParseResults<TagArgs>
        | [<CliPrefix(CliPrefix.None)>] Clean of ParseResults<CleanArgs>
        | [<CliPrefix(CliPrefix.None)>] Required of ParseResults<RequiredSubcommand>
        | [<CliPrefix(CliPrefix.None)>] Unrecognized of ParseResults<GatherUnrecognizedSubcommand>
        | [<CliPrefix(CliPrefix.None)>] Multiple_Mandatories of ParseResults<MultipleMandatoriesSubCommand>
        | [<SubCommand; CliPrefix(CliPrefix.None)>] Nullary_Sub
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Verbose -> "be verbose."
                | Working_Directory _ -> "specify a working directory."
                | Listener _ -> "specify a listener."
                | Mandatory_Arg _ -> "a mandatory argument."
                | Unique_Arg _ -> "a unique argument."
                | Rest_Arg _ -> "an argument that consumes all remaining command line tokens."
                | Data _ -> "pass raw data in base64 format."
                | Dir _ -> "Project directory to place the config & database in."
                | Flex_Equals_Assignment _ -> "An equals assignment which can also be used with a space separator"
                | Flex_Equals_Assignment_With_Option _ -> "Flex_Equals_Assignment but with optional parameter type"
                | Flex_Colon_Assignment _ -> "A colon assignment which can also be used with a space separator"
                | Log_Level _ -> "set the log level."
                | Float32_Arg _ -> "Some float32"
                | Float64_Arg _ -> "Some float64"
                | Decimal_Arg _ -> "Some decimal"
                | Detach -> "detach daemon from console."
                | Assignment _ -> "assign with colon operation."
                | Enum _ -> "assign from three possible values."
                | Enumeration _ -> "assign from three possible values."
                | Env _ -> "assign environment variables."
                | Main _ -> "main command."
                | CustomAppConfig _ -> "parameter with custom AppConfig key."
                | First_Parameter _ -> "parameter that has to appear at beginning of command line args."
                | Last_Parameter _ -> "parameter that has to appear at end of command line args."
                | Push _ -> "push changes"
                | Checkout _ -> "checkout ref"
                | Tag _ -> "tag"
                | Clean _ -> "clean state"
                | Required _ -> "required subcommand"
                | Unrecognized _ -> "unrecognized subcommand"
                | Multiple_Mandatories _ -> "multiple mandatories subcommand"
                | Nullary_Sub -> "nullary subcommand"
                | List _ -> "variadic params"
                | Optional _ -> "optional params"
                | A | B | C -> "misc arguments"

    let parser = ArgumentParser.Create<Argument> (programName = "gadget")
    let parseFunc ignoreMissing f = parser.ParseConfiguration(ConfigurationReader.FromFunction f, ignoreMissing)
