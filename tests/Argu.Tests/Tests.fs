namespace Argu.Tests

#nowarn "44"

open System
open System.IO
open Xunit
open FSharp.Quotations
open Swensen.Unquote.Assertions

open Argu

module ``Argu Tests`` =

    type PushArgs =
        | Remote of name:string
        | Branch of name:string
    with
        interface IArgParserTemplate with
            member this.Usage = "push"

    [<CliPrefix(CliPrefix.Dash)>]
    type CleanArgs =
        | D
        | F
        | X
    with
        interface IArgParserTemplate with
            member this.Usage = "clean"

    type Argument =
        | Working_Directory of string
        | [<AppSettingsSeparator(':')>] Listener of host:string * port:int
        | [<Mandatory>] Mandatory_Arg of bool
        | [<Unique>] Unique_Arg of bool
        | [<Rest>][<ParseCSV>] Rest_Arg of int
        | Data of int * byte []
        | Log_Level of int
        | [<AltCommandLine("/D", "-D", "-z")>] Detach
        | [<CustomAppSettings "Foo">] CustomAppConfig of string * int
        | [<EqualsAssignment>] Assignment of string
        | [<EqualsAssignment>] Env of key:string * value:string
        | [<First>] First_Parameter of string
        | Optional of int option
        | List of int list
        | [<CliPrefix(CliPrefix.Dash)>] A
        | [<CliPrefix(CliPrefix.Dash)>] B
        | [<CliPrefix(CliPrefix.Dash)>] C
        | [<CliPrefix(CliPrefix.None)>] Push of ParseResult<PushArgs>
        | [<CliPrefix(CliPrefix.None)>] Clean of ParseResult<CleanArgs>
    with
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Working_Directory _ -> "specify a working directory."
                | Listener _ -> "specify a listener."
                | Mandatory_Arg _ -> "a mandatory argument."
                | Unique_Arg _ -> "a unique argument."
                | Rest_Arg _ -> "an argument that consumes all remaining command line tokens."
                | Data _ -> "pass raw data in base64 format."
                | Log_Level _ -> "set the log level."
                | Detach _ -> "detach daemon from console."
                | Assignment _ -> "assign with equals operation."
                | Env _ -> "assign environment variables."
                | CustomAppConfig _ -> "parameter with custom AppConfig key."
                | First_Parameter _ -> "parameter that has to appear at beginning of command line args."
                | Push _ -> "push changes"
                | Clean _ -> "clean state"
                | List _ -> "variadic params"
                | Optional _ -> "optional params"
                | A | B | C -> "misc arguments"

    let parser = ArgumentParser.Create<Argument> ()
    let parseFunc ignoreMissing f = parser.ParseConfiguration(ConfigurationReader.FromFunction f, ignoreMissing)

    [<Fact>]
    let ``Simple command line parsing`` () =
        let args = 
            [| "--first-parameter" ; "bar" ; "--mandatory-arg" ; "true" ; "-D" ; 
                "--listener" ; "localhost" ; "8080" ; "--log-level" ; "2" |]

        let expected_outcome = [ First_Parameter "bar" ; Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let results = parser.ParseCommandLine args

        test <@ results.GetAllResults() = expected_outcome @>
        test <@ results.Contains <@ Detach @> @>
        test <@ results.GetResult <@ Listener @> = ("localhost", 8080) @>
        test <@ results.GetResults <@ Log_Level @> = [2] @>
        test <@ results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) = 3 @>

    [<Fact>]
    let ``Simple AppSettings parsing`` () =
        let args = [ Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ] |> List.sortBy tagOf
        let xmlSource = parser.PrintAppSettingsArguments args
        let xmlFile = Path.GetTempFileName()
        do File.WriteAllText(xmlFile, xmlSource)
        let reader = ConfigurationReader.FromAppSettingsFile(xmlFile)
        let results = parser.ParseConfiguration(reader)

        test <@ results.GetAllResults () = args @>

        test <@ results.Contains <@ Detach @> @>
        test <@ results.GetResult <@ Listener @> = ("localhost", 8080) @>
        test <@ results.GetResults <@ Log_Level @> = [2] @>
        test <@ results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) = 3 @>

    [<Fact>]
    let ``AppSettings CSV parsing`` () =
        let results = parseFunc true (function "rest arg" -> Some("1,2,3,4,5") | _ -> None)
        test <@ results.GetResults <@ Rest_Arg @> = [1 .. 5] @>

    [<Fact>]
    let ``AppSettings Flag parsing`` () =
        let results = parseFunc true (function "a" -> Some("true") | "b" -> Some("false") | _ -> None)
        test <@ results.Contains <@ A @> @>
        test <@ results.Contains <@ B @> |> not @>
        test <@ results.Contains <@ C @> |> not @>

    [<Fact>]
    let ``AppSettings multi-parameter parsing 1`` () =
        let results = parseFunc true (function "env" -> Some("key,value") | _ -> None)
        test <@ results.GetResult <@ Env @> = ("key", "value") @>

    [<Fact>]
    let ``AppSettings multi-parameter parsing 2`` () =
        let results = parseFunc true (function "listener" -> Some("localhost:80") | _ -> None)
        test <@ results.GetResult <@ Listener @> = ("localhost", 80) @>

    [<Fact>]
    let ``AppSettings Optional param`` () =
        let results = parseFunc true (function "optional" -> Some "42" | _ -> None)
        test <@ results.GetResult <@ Optional @> = (Some 42) @>

    [<Fact>]
    let ``AppSettings List param populated`` () =
        let results = parseFunc true (function "list" -> Some "1,2,3,4,5" | _ -> None)
        test <@ results.GetResult <@ List @> = [1 .. 5] @>

    [<Fact>]
    let ``AppSettings List param single`` () =
        let results = parseFunc true (function "list" -> Some "42" | _ -> None)
        test <@ results.GetResult <@ List @> = [42] @>
        

    [<Fact>]
    let ``Help String`` () =
        raises<ArguParseException> <@ parser.ParseCommandLine [| "--help" |] @>

    [<Fact>]
    let ``Missing Mandatory parameter`` () =
        raises<ArguParseException> <@ parser.ParseCommandLine [| "-D" |] @>

    [<Fact>]
    let ``Unique parameter specified once`` () =
        let result = parser.ParseCommandLine([| "--unique-arg" ; "true" |], ignoreMissing = true)
        test <@ result.GetResult <@ Unique_Arg @> @>

    [<Fact>]
    let ``Unique parameter specified twice`` () =
        raises<ArguParseException> <@ parser.ParseCommandLine([| "--unique-arg" ; "true" ; "--unique-arg" ; "false" |], ignoreMissing = true) @>

    [<Fact>]
    let ``First Parameter not placed at beggining`` () =
        raises<ArguParseException> <@ parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] @>

    [<Fact>]
    let ``Rest Parameter`` () =
        let args = [|1..100|] |> Array.map string |> Array.append [| "--mandatory-arg" ; "true" ; "--rest-arg" |]
        let result = parser.ParseCommandLine args
        test <@ result.GetResults <@ Rest_Arg @> = [1..100] @>

    [<Fact>]
    let ``Multiple AltCommandLine`` () =
        let args = [| "--mandatory-arg" ; "true" ; "-z" |]
        let results = parser.ParseCommandLine args
        test <@ results.Contains <@ Detach @> @>

    [<Fact>]
    let ``Usage documents explicitly named argument union case values`` () =
        let usage = parser.PrintUsage()
        test <@ usage.Contains "<host>" && usage.Contains "<port>" @>

    [<Fact>]
    let ``Parse byte[] parameters`` () =
        let bytes = [|1uy .. 255uy|]
        let args = parser.PrintCommandLineArguments [ Mandatory_Arg false ; Data(42, bytes) ]
        let results = parser.ParseCommandLine args
        test <@ let _,bytes' = results.GetResult <@ Data @> in bytes' = bytes @>

    [<Fact>]
    let ``Parse equals assignment`` () =
        let arg = [ Assignment "foo bar" ]
        let clp = parser.PrintCommandLineArguments arg
        let result = parser.Parse(clp, ignoreMissing = true)
        test <@ result.GetResult <@ Assignment @> = "foo bar" @>

    [<Fact>]
    let ``Parse key-value equals assignment`` () =
        let arg = [ Env("foo", "bar") ]
        let clp = parser.PrintCommandLineArguments arg
        let result = parser.Parse(clp, ignoreMissing = true)
        test <@ result.GetResult <@ Env @> = ("foo", "bar") @>


    [<Fact>]
    let ``Ignore Unrecognized parameters`` () =
        let args = 
            [| "--first-parameter" ; "bar" ; "--junk-param" ; "42" ; "--mandatory-arg" ; "true" ; "-D" ; 
                "--listener" ; "localhost" ; "8080" ; "--log-level" ; "2" |]

        let expected_outcome = [ First_Parameter "bar" ; Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let results = parser.ParseCommandLine (args, ignoreUnrecognized = true)
        test <@ results.GetAllResults() = expected_outcome @>

        test <@ results.Contains <@ Detach @> @>
        test <@ results.GetResult <@ Listener @> = ("localhost", 8080) @>
        test <@ results.GetResults <@ Log_Level @> = [2] @>
        test <@ results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) = 3 @>


    [<Fact>]
    let ``Should recognize grouped switches`` () =
        let args = [| "-cba"; "-cc" |]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        test <@ results.GetAllResults() = [C; B; A; C; C] @>

    [<Fact>]
    let ``Simple subcommand parsing 1`` () =
        let args = [|"push"; "--remote" ; "origin" ; "--branch" ; "master"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Push @>
        test <@ match results.TryGetSubCommand() with Some (Push _) -> true | _ -> false @>
        test <@ nested.GetAllResults() = [Remote "origin" ; Branch "master"] @>

    [<Fact>]
    let ``Simple subcommand parsing 2`` () =
        let args = [|"clean"; "-fdx"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Clean @>
        test <@ match results.TryGetSubCommand() with Some (Clean _) -> true | _ -> false @>
        test <@ nested.GetAllResults() = [F; D; X] @>

    [<Fact>]
    let ``Unrecognized CLI params`` () =
        let args = [| "--mandatory-arg" ; "true" ; "foobar" ; "-z" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        test <@ results.UnrecognizedCliParams = ["foobar"] @>
        test <@ results.Contains <@ Detach @> @>

    [<Fact>]
    let ``Nested unrecognized CLI params`` () =
        let args = [| "push" ; "foobar" ; "--branch" ; "master" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true, ignoreMissing = true)
        let nested = results.GetResult <@ Push @>
        test <@ nested.UnrecognizedCliParams = ["foobar"] @>
        test <@ nested.Contains <@ Branch @> @>
        test <@ results.UnrecognizedCliParams = [] @>

    [<Fact>]
    let ``Should allow '--help' before first args`` () =
        let result = parser.Parse([| "--help" ; "--first-parameter" ; "bar"|], raiseOnUsage = false, ignoreMissing = true)
        test <@ result.IsUsageRequested @>

    [<Fact>]
    let ``Should allow '--help' before mandatory args`` () =
        let result = parser.Parse([| "--help" ; "--mandatory-arg" ; "true"|], raiseOnUsage = false)
        test <@ result.IsUsageRequested @>


    [<Fact>]
    let ``Optional parameter: None`` () =
        let result = parser.Parse([|"--optional" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Optional @> = None @>

    [<Fact>]
    let ``Optional parameter: Some`` () =
        let result = parser.Parse([|"--optional" ; "42" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Optional @> = (Some 42) @>

    [<Fact>]
    let ``List parameter: empty`` () =
        let result = parser.Parse([|"--list" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ List @> = [] @>

    [<Fact>]
    let ``List parameter: singleton`` () =
        let result = parser.Parse([|"--list" ; "42" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ List @> = [42] @>

    [<Fact>]
    let ``List parameter: multiple args`` () =
        let result = parser.Parse([|"--mandatory-arg" ; "true"; "--list" ; "1" ; "2" ; "3" ; "4" ; "5" |])
        test <@ result.GetResult <@ List @> = [1 .. 5] @>

    [<Fact>]
    let ``Get all subcommand parsers`` () =
        let subcommands = parser.GetSubCommandParsers()
        test <@ subcommands.Length = 2 @>
        test <@ subcommands |> List.forall (fun sc -> sc.IsSubCommandParser) @>

    [<Fact>]
    let ``Get specific subcommand parser`` () =
        let subcommand = parser.GetSubCommandParser <@ Push @>
        test <@ subcommand.IsSubCommandParser @>


    type ConflictingCliNames =
        | [<CustomCommandLine "foo">] Foo of int
        | [<AltCommandLine "foo">] Bar of string
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    type ConflictingAppSettingsNames =
        | [<CustomAppSettings "foo">]Foo of int
        | [<CustomAppSettings "foo">] Bar of string
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    type RecursiveArgument1 =
        | Rec1 of ParseResult<RecursiveArgument2>
    with
        interface IArgParserTemplate with
            member a.Usage = "foo"

    and RecursiveArgument2 =
        | Rec2 of ParseResult<RecursiveArgument3>
    with
        interface IArgParserTemplate with
            member a.Usage = "bar"

    and RecursiveArgument3 =
        | Rec3 of ParseResult<RecursiveArgument1>
    with
        interface IArgParserTemplate with
            member a.Usage = "baz"

    type GenericArgument<'T> =
        | Bar of 'T
    with
        interface IArgParserTemplate with
            member a.Usage = "baz"

    [<Fact>]
    let ``Identify conflicting CLI identifiers`` () =
        raises<ArguException> <@ ArgumentParser.Create<ConflictingCliNames>() @>

    [<Fact>]
    let ``Identify conflicting AppSettings identifiers`` () =
        raises<ArguException> <@ ArgumentParser.Create<ConflictingAppSettingsNames>() @>

    [<Fact>]
    let ``Identify recursive subcommands`` () =
        raises<ArguException> <@ ArgumentParser.Create<RecursiveArgument1>() @>

    [<Fact>]
    let ``Identify generic arguments`` () =
        raises<ArguException> <@ ArgumentParser.Create<GenericArgument<string>>() @>


    [<CliPrefix(CliPrefix.Dash)>]
    type ArgumentSingleDash =
        | Argument of string
        | Levels_Deep of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"


    [<Fact>]
    let ``Use single dash prefix as default`` () =
        let parser = ArgumentParser.Create<ArgumentSingleDash>("usage string")
        let args = 
            [| "-argument" ; "bar" ; "-levels-deep" ; "3" |]

        let expected_outcome = set [ Argument "bar" ; Levels_Deep 3 ]
        let results = parser.ParseCommandLine args
        test <@ results.GetAllResults() |> set = expected_outcome @>

        test <@ results.Contains <@ Argument @> @>
        test <@ results.GetResult <@ Levels_Deep @> = 3 @>



    [<CliPrefix(CliPrefix.None)>]
    type ArgumentNoDash =
        | Argument of string
        | Levels_Deep of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"


    [<Fact>]
    let ``Use no prefix as default`` () =
        let parser = ArgumentParser.Create<ArgumentNoDash>("usage string")
        let args = [| "argument" ; "bar" ; "levels-deep" ; "3" |]

        let expected_outcome = set [ Argument "bar" ; Levels_Deep 3 ]
        let results = parser.ParseCommandLine args
        test <@ results.GetAllResults() |> set = expected_outcome @>

        test <@ results.Contains <@ Argument @> @>
        test <@ results.GetResult <@ Levels_Deep @> = 3 @>

    [<Fact>]
    let ``Should fail if EqualsAssignment missing assignment.`` () =
        raises<ArguParseException> <@ parser.ParseCommandLine [|"--assignment"; "value"|] @>

    [<Fact>]
    let ``Slash in Commandline`` () =
        let args = [| "--mandatory-arg" ; "true" ; "/D" |]
        let results = parser.ParseCommandLine args
        test <@ results.Contains <@ Detach @> @>
    
    [<Fact>]
    let ``Should fail wenn Usage, Mandatory and raiseOnUsage = true`` () =
         raises<ArguParseException> <@ parser.ParseCommandLine ([|"--help"|], raiseOnUsage = true) @>

    [<Fact>]
    let ``Usage, Mandatory and raiseOnusage = false`` () =
        let args = [| "--help" |]
        let results = parser.ParseCommandLine (args, raiseOnUsage = false)
        test <@ results.IsUsageRequested @>


    [<HelpFlags("--my-help")>]
    [<HelpDescription("waka jawaka")>]
    type ArgumentWithAltHelp =
        | AltHelp1 of string
        | AltHelp2 of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<DisableHelpFlags>]
    type NoHelp =
        | NoHelp1 of string
        | NoHelp2 of int
    with
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<Fact>]
    let ``Custom Help attribute`` () =
        let parser = ArgumentParser.Create<ArgumentWithAltHelp>()
        let results = parser.Parse([|"--my-help"|], raiseOnUsage = false)
        test <@ results.IsUsageRequested @>

    [<Fact>]
    let ``Custom Help Description attribute`` () =
        let parser = ArgumentParser.Create<ArgumentWithAltHelp>()
        test <@ parser.PrintUsage().Contains "waka jawaka" @>

    [<Fact>]
    let ``Custom Help attribute should not use default helper`` () =
        let parser = ArgumentParser.Create<ArgumentWithAltHelp>()
        let results = parser.Parse([|"--help"|], raiseOnUsage = false, ignoreUnrecognized = true)
        test <@ not results.IsUsageRequested @>

    [<Fact>]
    let ``Disable Help attribute`` () =
        let parser = ArgumentParser.Create<NoHelp>()
        let result = parser.Parse([|"--help"|], raiseOnUsage = false, ignoreUnrecognized = true)
        test <@ not result.IsUsageRequested @>

    [<Fact>]
    let ``Fail on malformed case constructors`` () =
        let result = parser.ToParseResult []
        raises<ArgumentException> <@ result.Contains <@ fun (y : string) -> Log_Level 42 @> @>
        raises<ArgumentException> <@ result.Contains <@ fun (y, x) -> Data(x,y) @> @>
        raises<ArgumentException> <@ result.Contains <@ fun x -> () ; Log_Level x @> @>
        raises<ArgumentException> <@ result.Contains <@ let wrapper = List in wrapper @> @>
        let wrapper = List
        raises<ArgumentException> <@ result.Contains <@ wrapper @> @>