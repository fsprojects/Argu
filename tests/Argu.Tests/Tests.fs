namespace Argu.Tests

#nowarn "44"

open System
open System.IO
open System.Globalization
open Xunit
open Swensen.Unquote

open Argu

module ``Argu Tests Main List`` =

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
                | Nullary_Sub -> "nullary subcommand"
                | List _ -> "variadic params"
                | Optional _ -> "optional params"
                | A | B | C -> "misc arguments"

    let parser = ArgumentParser.Create<Argument> (programName = "gadget")
    let parseFunc ignoreMissing f = parser.ParseConfiguration(ConfigurationReader.FromFunction f, ignoreMissing)

    [<Fact>]
    let ``Numeric decimal separators parsing is culture invariant``() =
        CultureInfo.CurrentUICulture <- CultureInfo.CurrentUICulture.Clone() :?> CultureInfo
        CultureInfo.CurrentUICulture.NumberFormat.NumberDecimalSeparator <- ","

        let args =
            [| "--float32-arg"; "1.2"
               "--float64-arg"; "2.3"
               "--decimal-arg"; "3.4"
               "--mandatory-arg"; "true" |]

        let expected = [ Float32_Arg 1.2f; Float64_Arg 2.3; Decimal_Arg 3.4m; Mandatory_Arg true ]
        let results = parser.ParseCommandLine args

        test <@ results.GetAllResults() = expected @>

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
    let ``Simple AppSettings contains usage comments`` () =
        let args = [ Mandatory_Arg true ; Detach ; Listener ("localhost", 8080) ; Log_Level 2 ]
        let xmlSource = parser.PrintAppSettingsArguments args
        let usages = List.map (fun a -> (a :> IArgParserTemplate).Usage) args
        
        test <@ xmlSource.Contains usages[0] = true @>
        test <@ xmlSource.Contains usages[1] = true @>
        test <@ xmlSource.Contains usages[2] = true @>

    [<Fact>]
    let ``AppSettings CSV parsing`` () =
        let results = parseFunc true (function "rest arg" -> Some("1,2,3,4,5") | _ -> None)
        test <@ results.GetResults Rest_Arg = [1 .. 5] @>

    [<Fact>]
    let ``AppSettings Flag parsing`` () =
        let results = parseFunc true (function "a" -> Some("true") | "b" -> Some("false") | _ -> None)
        test <@ results.Contains A @>
        test <@ results.Contains B |> not @>
        test <@ results.Contains C |> not @>

    [<Fact>]
    let ``AppSettings multi-parameter parsing 1`` () =
        let results = parseFunc true (function "env" -> Some("key,value") | _ -> None)
        test <@ results.GetResult Env = ("key", "value") @>

    [<Fact>]
    let ``AppSettings multi-parameter parsing 2`` () =
        let results = parseFunc true (function "listener" -> Some("localhost:80") | _ -> None)
        test <@ results.GetResult Listener = ("localhost", 80) @>

    [<Fact>]
    let ``AppSettings Optional param`` () =
        let results = parseFunc true (function "optional" -> Some "42" | _ -> None)
        test <@ results.GetResult Optional = (Some 42) @>

    [<Fact>]
    let ``AppSettings List param populated`` () =
        let results = parseFunc true (function "list" -> Some "1,2,3,4,5" | _ -> None)
        test <@ results.GetResult List = [1 .. 5] @>

    [<Fact>]
    let ``AppSettings List param single`` () =
        let results = parseFunc true (function "list" -> Some "42" | _ -> None)
        test <@ results.GetResult List = [42] @>
        

    [<Fact>]
    let ``Help String`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine [| "--help" |] @>
                                        (fun e -> <@ e.Message.Contains "USAGE:" @>)

    [<Fact>]
    let ``Missing Mandatory parameter`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine [| "-D" |] @>
                                        (fun e -> <@ e.FirstLine.Contains "missing parameter" @>)


    // Test the Turkish dot-less 'i', when converting the capital 'I' in the Union below this
    // will incorrectly
    type LocaleTurkish =
        | Install of bool
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<Fact>]
    let ``CLIArguments Locale`` () =
        let originalCulture = System.Threading.Thread.CurrentThread.CurrentCulture
        try
            System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo("tr-TR")
            let parser2 = ArgumentParser.Create<LocaleTurkish> (programName = "gadget")
            let result = parser2.ParseCommandLine([| "--install"; "true" |], ignoreMissing = true)
            test <@ result.GetResult <@ Install @> @>
        finally
            System.Threading.Thread.CurrentThread.CurrentCulture <- originalCulture

    [<Fact>]
    let ``Unique parameter specified once`` () =
        let result = parser.ParseCommandLine([| "--unique-arg" ; "true" |], ignoreMissing = true)
        test <@ result.GetResult <@ Unique_Arg @> @>

    [<Fact>]
    let ``Unique parameter specified twice`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine([| "--unique-arg" ; "true" ; "--unique-arg" ; "false" |], ignoreMissing = true) @>
                                        (fun e -> <@ e.Message.Contains "more than once" @>)

    [<Fact>]
    let ``First Parameter not placed at beginning`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] @>
                                        (fun e -> <@ e.Message.Contains "should precede all other" @>)
                                        

    [<Fact>]
    let ``Rest Parameter`` () =
        let args = [|1..100|] |> Array.map string |> Array.append [| "--mandatory-arg" ; "true" ; "--rest-arg" |]
        let result = parser.ParseCommandLine args
        test <@ result.GetResults Rest_Arg = [1..100] @>

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
        test <@ let _,bytes' = results.GetResult Data in bytes' = bytes @>

    [<Fact>]
    let ``Parse colon assignment 1`` () =
        let args = [| "--assignment:foobar" |]
        let result = parser.Parse(args, ignoreMissing = true)
        test <@ result.GetResult <@ Assignment @> = "foobar" @>

    [<Fact>]
    let ``Parse colon assignment 2`` () =
        let arg = [ Assignment "foo bar" ]
        let clp = parser.PrintCommandLineArguments arg
        let result = parser.Parse(clp, ignoreMissing = true)
        test <@ result.GetResult <@ Assignment @> = "foo bar" @>

    [<Fact>]
    let ``Parse key-value equals assignment`` () =
        let arg = [ Env("foo", "bar") ]
        let clp = parser.PrintCommandLineArguments arg
        let result = parser.Parse(clp, ignoreMissing = true)
        test <@ result.GetResult Env = ("foo", "bar") @>

    [<Fact>]
    let ``Parse key-value equals assignment 2`` () =
        let result = parser.Parse([|"--env"; "foo==bar"|], ignoreMissing = true)
        test <@ result.GetResult <@ Env @> = ("foo", "=bar") @>

    [<Fact>]
    let ``Parse equals assignment`` () =
        let result = parser.Parse([|"--dir=../../my-relative-path"|], ignoreMissing = true)
        test <@ result.GetResult Dir = "../../my-relative-path" @>

    [<Fact>]
    let ``Parse equals assignment 2`` () =
        let result = parser.Parse([|"--dir==foo"|], ignoreMissing = true)
        test <@ result.GetResult Dir = "=foo" @>
        
    [<Fact>]
    let ``Parse equals or space assignment with equals`` () =
        let result = parser.Parse([|"--flex-equals-assignment=../../my-relative-path"; "--dir==foo"|], ignoreMissing = true)
        test <@ result.GetResult Flex_Equals_Assignment = "../../my-relative-path" @>
        
    [<Fact>]
    let ``Parse equals or space assignment with colon fails`` () =
        raises<ArguParseException> <@ parser.Parse([|"--flex-equals-assignment:../../my-relative-path"; "--dir==foo"|], ignoreMissing = true) @>
        
    [<Fact>]
    let ``Parse equals or space assignment with space`` () =
        let result = parser.Parse([|"--flex-equals-assignment"; "../../my-relative-path"; "--dir==foo"|], ignoreMissing = true)
        test <@ result.GetResult Flex_Equals_Assignment = "../../my-relative-path" @>
        
    [<Fact>]
    let ``Parse equals or space assignment with space and optional type`` () =
        let result = parser.Parse([|"--flex-equals-assignment-with-option"; "../../my-relative-path"; "--dir==foo"|], ignoreMissing = true)
        test <@ result.GetResult Flex_Equals_Assignment_With_Option = Some "../../my-relative-path" @>

    [<Fact>]
    let ``Parse colon or space assignment with colon`` () =
        // No need to test space assignment or assignment failure, as EitherSpaceOrEqualsAssignmentAttribute and
        // EitherSpaceOrColonAssignmentAttribute share the same underlying implementation.
        let result = parser.Parse([|"--flex-colon-assignment:../../my-relative-path"; "--dir==foo"|], ignoreMissing = true)
        test <@ result.GetResult Flex_Colon_Assignment = "../../my-relative-path" @>
    
    type DisallowedAssignmentArgs =
    | [<EqualsAssignmentOrSpaced>] [<EqualsAssignment>] Flex_Equals_Assignment of string
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Flex_Equals_Assignment _ -> "Disallowed attribute combination"

    [<Fact>]
    let ``Disallowed equals assignment combination throws`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<DisallowedAssignmentArgs> (programName = "gadget") @>
        
    type DisallowedArityWithAssignmentOrSpaced =
    | [<EqualsAssignmentOrSpaced>] Flex_Equals_Assignment of string * int
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Flex_Equals_Assignment _ -> "Disallowed attribute / arity combination"

    [<Fact>]
    let ``EqualsAssignmentOrSpaced and arity not one combination throws`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<DisallowedArityWithAssignmentOrSpaced> (programName = "gadget1") @>
    
    [<Fact>]
    let ``Should fail on incorrect assignment 1`` () =
        raises<ArguParseException> <@ parser.Parse([|"--dir:foo"|], ignoreMissing = true) @>


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
        test <@ results.GetResults Log_Level = [2] @>
        test <@ results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) = 3 @>

    [<Fact>]
    let ``Fail on misplaced First parameter`` () =
        let args = [|"--mandatory-arg" ; "true" ; "--first-parameter" ; "arg" |]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine args @>
                                        (fun e -> <@ e.FirstLine.Contains "should precede" @>)

    [<Fact>]
    let ``Fail on misplaced Last parameter`` () =
        let args = [|"--last-parameter" ; "arg" ; "--mandatory-arg"; "true"|]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine args @>
                                        (fun e -> <@ e.FirstLine.Contains "should appear after" @>)

    [<Fact>]
    let ``Should recognize grouped switches`` () =
        let args = [|"-cba" ; "-cc"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        test <@ results.GetAllResults() = [C; B; A; C; C] @>

    [<Fact>]
    let ``Simple subcommand parsing 1`` () =
        let args = [|"push" ; "-f" ; "origin" ; "master"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Push @>
        test <@ match results.TryGetSubCommand() with Some (Push _) -> true | _ -> false @>
        test <@ nested.GetResults <@ Remote @> = [("origin", "master")] @>
        test <@ nested.Contains <@ Force @> @>

    [<Fact>]
    let ``Simple subcommand parsing 2`` () =
        let args = [|"clean"; "-fdx"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult Clean
        test <@ match results.TryGetSubCommand() with Some (Clean _) -> true | _ -> false @>
        test <@ nested.GetAllResults() = [F; D; X] @>

    [<Fact>]
    let ``Main command parsing should fail on missing parameters`` () =
        let args = [|"push" ; "origin" |]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine args @>
                                    (fun e -> <@ e.FirstLine.Contains "must be followed by <branch name>" @>)

    [<Fact>]
    let ``Main command parsing should fail on missing mandatory sub command parameter`` () =
        let args = [|"--mandatory-arg" ; "true" ; "checkout"  |]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine args @>
                                    (fun e -> <@ e.FirstLine.Contains "--branch" @>)

    [<Fact>]
    let ``Main command parsing should fail on missing mandatory sub command's sub command parameter`` () =
        let args = [|"--mandatory-arg"; "true"; "tag"; "--new"; |]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine args @>
                                    (fun e -> <@ e.FirstLine.Contains "--name" @>)

    [<Fact>]
    let ``Main command parsing should allow trailing arguments`` () =
        let args = [|"push" ; "origin" ; "master" ; "-f" |]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Push @>
        test <@ nested.GetResults <@ Remote @> = [("origin", "master")] @>
        test <@ nested.Contains <@ Force @> @>

    [<Fact>]
    let ``Main command parsing should not permit interstitial arguments`` () =
        let args = [|"push" ; "origin" ; "-f" ; "master"|]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine(args, ignoreMissing = true) @>
                                    (fun e -> <@ e.FirstLine.Contains "but was '-f'" @>)

        let results = parser.ParseCommandLine(args, ignoreMissing = true, ignoreUnrecognized = true)
        let nested = results.GetResult <@ Push @>
        test <@ nested.Contains <@ Force @> @>
        test <@ nested.Contains <@ Remote @> |> not @>
        test <@ nested.UnrecognizedCliParams = ["origin" ; "master"] @>

    [<Fact>]
    let ``Main command taking list of arguments`` () =
        let args = [|"--mandatory-arg" ; "true" ; "a" ; "b" ; "c" ; "ff" ; "d" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        test <@ results.GetResult Main = ['a' ; 'b' ; 'c'] @>

    [<Fact>]
    let ``SubParsers should correctly handle inherited params`` () =
        let subParser = parser.GetSubCommandParser <@ Clean @>
        let result = subParser.ParseCommandLine [|"-fdxv"|]
        test <@ match result.UnrecognizedCliParseResults with [:? Argument as c ] -> c = Verbose | _ -> false @>

    [<Fact>]
    let ``Doubly nested subcommand parsing`` () =
        let args = [|"required" ; "--foo" ; "sub" ; "-fdx" |]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested  = results.GetResult <@ Required @>
        let nested' = nested.GetResult <@ Sub @>
        test <@ nested'.Contains <@ F @> @>

    [<Fact>]
    let ``Required subcommand attribute should fail on missing subcommand`` () =
        let args = [|"required" ; "--foo" |]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine(args, ignoreMissing = true) @>
                                        (fun e -> <@ e.FirstLine.Contains "subcommand" @>)

    [<Fact>]
    let ``Nullary subcommand`` () =
        let args = [|"nullary-sub"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        test <@ results.TryGetSubCommand() = Some Nullary_Sub @>

    [<Fact>]
    let ``Required subcommand should succeed on nullary subcommand`` () =
        let args = [|"required"; "null-sub"|]
        let results = parser.ParseCommandLine(args, ignoreMissing = true)
        let nested = results.GetResult <@ Required @>
        test <@ nested.TryGetSubCommand() = Some Null_Sub @>

    [<Fact>]
    let ``Calling both a nullary subcommand a normal one should fail`` () =
        let args = [|"required"; "null-sub"; "sub"; "-fdx"|]
        raisesWith<ArguParseException> <@ parser.ParseCommandLine(args, ignoreMissing = true) @>
                                        (fun e -> <@ e.FirstLine.Contains "subcommand" @>)

    [<Fact>]
    let ``GatherUnrecognized attribute`` () =
        let args = [|"--mandatory-arg" ; "true" ; "unrecognized" ; "uarg1" ; "--switch1" ; "uarg2"|]
        let results = parser.ParseCommandLine(args)
        let sub = results.GetResult <@ Unrecognized @>
        test <@ sub.UnrecognizedCliParams = [] @>
        test <@ sub.Contains <@ Switch1 @> @>
        test <@ sub.GetResults <@ Unrec @> = [ "uarg1" ; "uarg2" ] @>

    [<Fact>]
    let ``Unrecognized CLI params`` () =
        let args = [| "--mandatory-arg" ; "true" ; "foobar" ; "-z" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        test <@ results.UnrecognizedCliParams = ["foobar"] @>
        test <@ results.Contains <@ Detach @> @>

    [<Fact>]
    let ``Unrecognized CLI param first`` () =
        let args = [| "f" ; "--mandatory-arg" ; "true" ; "-z"; "m"; "a"; "i"; "n" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        test <@ results.UnrecognizedCliParams = ["f"] @>
        test <@ results.Contains <@ Detach @> @>
        test <@ results.GetResult <@ Main @> = ['m'; 'a'; 'i'; 'n'] @>

    [<Fact>]
    let ``Nested unrecognized CLI params`` () =
        let args = [| "clean" ; "foobar" ; "-fdx" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true, ignoreMissing = true)
        let nested = results.GetResult <@ Clean @>
        test <@ nested.UnrecognizedCliParams = ["foobar"] @>
        test <@ nested.Contains <@ D @> @>
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
    let ``Optional parameter - None`` () =
        let result = parser.Parse([|"--optional" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Optional @> = None @>

    [<Fact>]
    let ``Optional parameter - Some`` () =
        let result = parser.Parse([|"--optional" ; "42" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Optional @> = (Some 42) @>

    [<Fact>]
    let ``Enumeration parameter parsing`` () =
        let result = parser.Parse([|"--enum" ; "second" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Enum @> = Enum.Second @>

    [<Fact>]
    let ``Enumeration parameter parsing (case-insensitive, allowing original case)`` () =
        let result = parser.Parse([|"--enum" ; "Third" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Enum @> = Enum.Third @>

    [<Fact>]
    let ``Enumeration parameter should fail on unsupported values`` () =
        raisesWith<ArguParseException> <@ parser.Parse([|"--enum" ; "first,second" ; "--mandatory-arg" ; "true"|]) @>
                                        (fun e -> <@ e.FirstLine.Contains "first|second|third" @>)

    [<Fact>]
    let ``F# Enumeration parameter parsing`` () =
        let result = parser.Parse([|"--enumeration=second" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Enumeration @> = Some Second @>

    [<Fact>]
    let ``F# Enumeration parameter parsing (case-insensitive, allowing original case)`` () =
        let result = parser.Parse([|"--enumeration=Third" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ Enumeration @> = Some Third @>

    [<Fact>]
    let ``F# Enumeration parameter should fail on unsupported values`` () =
        raisesWith<ArguParseException> <@ parser.Parse([|"--enumeration=foobar" ; "--mandatory-arg" ; "true"|]) @>
                                        (fun e -> <@ e.FirstLine.Contains "first|second|third" @>)

    [<Fact>]
    let ``List parameter - empty`` () =
        let result = parser.Parse([|"--list" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ List @> = [] @>

    [<Fact>]
    let ``List parameter - singleton`` () =
        let result = parser.Parse([|"--list" ; "42" ; "--mandatory-arg" ; "true"|])
        test <@ result.GetResult <@ List @> = [42] @>

    [<Fact>]
    let ``List parameter - multiple args`` () =
        let result = parser.Parse([|"--mandatory-arg" ; "true"; "--list" ; "1" ; "2" ; "3" ; "4" ; "5" |])
        test <@ result.GetResult <@ List @> = [1 .. 5] @>

    [<Fact>]
    let ``Get all subcommand parsers`` () =
        let subcommands = parser.GetSubCommandParsers()
        test <@ subcommands.Length = 6 @>
        test <@ subcommands |> List.forall (fun sc -> sc.IsSubCommandParser) @>

    [<Fact>]
    let ``Get specific subcommand parser`` () =
        let subcommand = parser.GetSubCommandParser <@ Push @>
        test <@ subcommand.IsSubCommandParser @>

    [<Fact>]
    let ``Inherited parameter simple`` () =
        let result1 = parser.Parse([|"-v"; "push"|], ignoreMissing = true)
        let result2 = parser.Parse([|"push"; "-v"|], ignoreMissing = true)
        test <@ result1.Contains <@ Verbose @> @>
        test <@ result1.Contains <@ Push @> @>
        test <@ result2.Contains <@ Verbose @> @>
        test <@ result2.Contains <@ Push @> @>

    [<Fact>]
    let ``Inherited parameter parametric`` () =
        let result1 = parser.Parse([|"--data" ; "42" ; "deadbeef" ; "push"|], ignoreMissing = true)
        let result2 = parser.Parse([|"push" ; "--data" ; "42" ; "deadbeef"|], ignoreMissing = true)
        test <@ result1.Contains <@ Data @> @>
        test <@ result1.Contains <@ Push @> @>
        test <@ result2.Contains <@ Data @> @>
        test <@ result2.Contains <@ Push @> @>

    [<Fact>]
    let ``Inherited parameter grouped`` () =
        let result1 = parser.Parse([|"-v" ; "clean" ; "-dx"|], ignoreMissing = true)
        let result2 = parser.Parse([|"clean" ; "-vdx"|], ignoreMissing = true)
        let subResult1 = result1.GetResult <@ Clean @>
        let subResult2 = result2.GetResult <@ Clean @>
        test <@ result1.Contains <@ Verbose @> @>
        test <@ result2.Contains <@ Verbose @> @>
        test <@ subResult1.Contains <@ X @> @>
        test <@ subResult2.Contains <@ X @> @>

    type ConflictingCliNames =
        | [<CustomCommandLine "foo">] Foo of int
        | [<AltCommandLine "foo">] Bar of string
        interface IArgParserTemplate with
            member a.Usage = "foo"

    type ConflictingAppSettingsNames =
        | [<CustomAppSettings "foo">]Foo of int
        | [<CustomAppSettings "foo">] Bar of string
        interface IArgParserTemplate with
            member a.Usage = "foo"

    type ConflictingInheritedCliName =
        | [<AltCommandLine("-f"); Inherit>] Force
        | Nested of ParseResults<CleanArgs>
        interface IArgParserTemplate with
            member _.Usage = "not tested"

    type RecursiveArgument1 =
        | Rec1 of ParseResults<RecursiveArgument2>
        interface IArgParserTemplate with
            member a.Usage = "foo"

    and RecursiveArgument2 =
        | Rec2 of ParseResults<RecursiveArgument3>
        interface IArgParserTemplate with
            member a.Usage = "bar"

    and RecursiveArgument3 =
        | Rec3 of ParseResults<RecursiveArgument1>
        interface IArgParserTemplate with
            member a.Usage = "baz"

    type GenericArgument<'T> =
        | Bar of 'T
        interface IArgParserTemplate with
            member a.Usage = "baz"

    [<Fact>]
    let ``Identify conflicting CLI identifiers`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<ConflictingCliNames>() @>
                                    (fun e -> <@ e.FirstLine.Contains "conflicting CLI" @>)

    [<Fact>]
    let ``Identify conflicting inherited CLI identifiers`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<ConflictingInheritedCliName>() @>
                                    (fun e -> <@ e.FirstLine.Contains "conflicting CLI" @>)

    [<Fact>]
    let ``Identify conflicting AppSettings identifiers`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<ConflictingAppSettingsNames>() @>
                                    (fun e -> <@ e.FirstLine.Contains "conflicting AppSettings" @>)

    [<Fact>]
    let ``Identify recursive subcommands`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<RecursiveArgument1>() @>
                                    (fun e -> <@ e.FirstLine.Contains "recursive" @>)

    [<Fact>]
    let ``Identify generic arguments`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<GenericArgument<string>>() @>
                                    (fun e -> <@ e.FirstLine.Contains "generic" @>)


    [<CliPrefix(CliPrefix.Dash)>]
    type ArgumentSingleDash =
        | Argument of string
        | Levels_Deep of int
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
        interface IArgParserTemplate with
            member a.Usage = "not tested here"


    [<Fact>]
    let ``Use no prefix as default`` () =
        let parser = ArgumentParser.Create<ArgumentNoDash>(programName = "gadget")
        let args = [| "argument" ; "bar" ; "levels-deep" ; "3" |]

        let expected_outcome = set [ Argument "bar" ; Levels_Deep 3 ]
        let results = parser.ParseCommandLine args
        test <@ results.GetAllResults() |> set = expected_outcome @>

        test <@ results.Contains <@ Argument @> @>
        test <@ results.GetResult <@ Levels_Deep @> = 3 @>

    [<Fact>]
    let ``Should fail if EqualsAssignment missing assignment.`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine([|"--assignment"; "value"|], ignoreMissing = true) @>
                                        (fun e -> <@ e.FirstLine.Contains "missing an assignment" @>)

    [<Fact>]
    let ``Slash in Commandline`` () =
        let args = [| "--mandatory-arg" ; "true" ; "/D" |]
        let results = parser.ParseCommandLine args
        test <@ results.Contains <@ Detach @> @>
    
    [<Fact>]
    let ``Should fail when Usage, Mandatory and raiseOnUsage = true`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine ([|"--help"|], raiseOnUsage = true) @>
                                        (fun e -> <@ e.FirstLine.Contains "USAGE:" @>)

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
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<DisableHelpFlags>]
    type NoHelp =
        | NoHelp1 of string
        | NoHelp2 of int
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

    [<DisableHelpFlags>]
    type NestedInherited =
        | [<Inherit>] Nested of ParseResults<NoHelp>
        interface IArgParserTemplate with
            member a.Usage = "not tested here"

    [<Fact>]
    let ``Fail on inherited nested union cases`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<NestedInherited>() @>
                                  (fun e -> <@ e.FirstLine.Contains "Inherit" @>)

    [<Fact>]
    let ``Fail on malformed case constructors`` () =
        let result = parser.ToParseResults []
        let wrapper = List
        raises<ArgumentException> <@ result.Contains <@ fun (_y : string) -> Log_Level 42 @> @>
        raises<ArgumentException> <@ result.Contains <@ fun (y, x) -> Data(x,y) @> @>
        raises<ArgumentException> <@ result.Contains <@ fun x -> () ; Log_Level x @> @>
        raises<ArgumentException> <@ result.Contains <@ let wrapper = List in wrapper @> @>
        raises<ArgumentException> <@ result.Contains <@ wrapper @> @>

    type SubCommand =
        | SubParameter
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | SubParameter -> "will be shown"

    type BaseCommand =
        | [<Inherit;Hidden>] BaseParameter
        | [<CustomCommandLine("sub")>] Sub of ParseResults<SubCommand>
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | BaseParameter -> "will be hidden"
                | Sub _ -> "subcommand"

    [<Fact>]
    let ``Hidden parameters are not printed in help text`` () =
        let parser = ArgumentParser.Create<SubCommand>()
        test <@ parser.PrintUsage().Contains "will be hidden" |> not @>

    [<Fact>]
    let ``Hidden inherited parameters are not printed in help text with subcommand`` () =
        let parser = ArgumentParser.Create<BaseCommand>()
        let results = parser.ParseCommandLine([|"sub"|])
        match results.GetSubCommand() with
        | Sub r ->
            test <@ r.Parser.PrintUsage().Contains "will be shown" @>
            test <@ r.Parser.PrintUsage().Contains "will be hidden" |> not @>
        | _ -> failwithf "never should get here"

module ``Argu Tests Main Primitive`` =

    type Exception with
        member inline x.FirstLine = 
            x.Message.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries).[0]

    type ArgumentPrimitive =
      | [<AltCommandLine("-v"); Inherit>] Verbose
      | Working_Directory of string
      | [<AppSettingsSeparator(':')>] Listener of host:string * port:int
      | [<Mandatory>] Mandatory_Arg of bool
      | [<Unique>] Unique_Arg of bool
      | [<Rest; ParseCSV>] Rest_Arg of int
      | [<MainCommand; Last; Unique>] Main of str:string
      | [<Inherit>] Data of int * byte []
      | Log_Level of int
      | [<AltCommandLine("/D", "-D", "-z")>] Detach
      | [<CustomAppSettings "Foo">] CustomAppConfig of string * int
      | [<ColonAssignment>] Assignment of string
      | [<EqualsAssignment>] Env of key:string * value:string
      | [<EqualsAssignment>] Dir of path:string
      | [<First>] First_Parameter of string
      | [<Last>] Last_Parameter of string
      | Optional of int option
      | List of int list
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
              | Log_Level _ -> "set the log level."
              | Detach -> "detach daemon from console."
              | Assignment _ -> "assign with colon operation."
              | Env _ -> "assign environment variables."
              | Main _ -> "main command."
              | CustomAppConfig _ -> "parameter with custom AppConfig key."
              | First_Parameter _ -> "parameter that has to appear at beginning of command line args."
              | Last_Parameter _ -> "parameter that has to appear at end of command line args."
              | List _ -> "variadic params"
              | Optional _ -> "optional params"


    let parser = ArgumentParser.Create<ArgumentPrimitive> (programName = "gadget")
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
    let ``Help String`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine [| "--help" |] @>
                                        (fun e -> <@ e.Message.Contains "USAGE:" @>)
    
    [<Fact>]
    let ``First Parameter not placed at beginning`` () =
        raisesWith<ArguParseException> <@ parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] @>
                                        (fun e -> <@ e.Message.Contains "should precede all other" @>)

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
        test <@ results.GetResults Log_Level = [2] @>
        test <@ results.PostProcessResult (<@ Log_Level @>, fun x -> x + 1) = 3 @>

    [<Fact>]
    let ``Unrecognized CLI params`` () =
        let args = [| "--mandatory-arg" ; "true" ; "foobar" ; "-z" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        test <@ results.UnrecognizedCliParams = ["foobar"] @>
        test <@ results.Contains <@ Detach @> @>

    [<Fact>]
    let ``Unrecognized CLI param first`` () =
        let args = [| "foobar" ; "--mandatory-arg" ; "true" ; "-z"; "main" |]
        let results = parser.ParseCommandLine(args, ignoreUnrecognized = true)
        test <@ results.UnrecognizedCliParams = ["foobar"] @>
        test <@ results.Contains <@ Detach @> @>
        test <@ results.GetResult <@ Main @> = "main" @>
        
    [<Fact>]
    let ``Trap defaulting function exceptions`` () =
        let results = parser.ParseCommandLine [| "--mandatory-arg" ; "true"; "command" |]
        let defThunk (): string = failwith "Defaulting Failed"
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, defThunk, showUsage = false) @>
            <| fun e -> <@ e.Message = "Defaulting Failed" && e.ErrorCode = ErrorCode.PostProcess @>
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, defThunk)  @>
            (fun e -> <@ e.Message.StartsWith "Defaulting Failed" && e.Message.Contains "--working-directory" @>)
 