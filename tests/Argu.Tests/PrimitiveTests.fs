module Argu.Tests.PrimitiveTests

open Swensen.Unquote
open Xunit

open Argu

type ArgumentPrimitive =
    | [<AltCommandLine("-v"); Inherit>] Verbose
    | Working_Directory of string
    | [<AppSettingsSeparator(':')>] Listener of host:string * port:int
    | [<Mandatory>] Mandatory_Arg of bool
    | [<Unique>] Unique_Arg of bool
#nowarn 44 // Obsolete attributes
    | [<Rest; ParseCSV>] Rest_Arg of int
#warnon 44
    | [<MainCommand; Last; Unique>] Main of str:string
    | [<Inherit>] Data of int * byte []
    | Log_Level of int
    | [<AltCommandLine("/D", "-D", "-z")>] Detach
    | [<CustomAppSettings "Foo">] CustomAppConfig of string * int
    | [<Separator ":">] Assignment of string
    | [<Separator "=">] Env of key:string * value:string
    | [<Separator "=">] Dir of path:string
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

let parser = ArgumentParser.Create<ArgumentPrimitive>(programName = "gadget")

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
    test <@ results.GetResult(<@ Log_Level @>, fun x -> x + 1) = 3 @>

[<Fact>]
let ``Help String`` () =
    raisesWith<ArguParseException>
        <@ parser.ParseCommandLine [| "--help" |] @>
        <| fun e -> <@ e.Message.Contains "USAGE:" @>

[<Fact>]
let ``First Parameter not placed at beginning`` () =
    raisesWith<ArguParseException>
        <@ parser.ParseCommandLine [| "--mandatory-arg" ; "true" ; "--first-parameter" ; "foo" |] @>
        <| fun e -> <@ e.Message.Contains "should precede all other" @>

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
    test <@ results.GetResult(<@ Log_Level @>, fun x -> x + 1) = 3 @>

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

module ``Traps for defaulting and or post-processing functions`` =
    let results = parser.ParseCommandLine [| "--mandatory-arg" ; "true"; "command" |]

    [<Fact>]
    let ``Trap defaulting function exceptions`` () =
        let failingDefThunk (): string = failwith "Defaulting Failed"
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, failingDefThunk, showUsage = false) @>
            <| fun e -> <@ e.Message = "Defaulting Failed" && e.ErrorCode = ErrorCode.PostProcess @>
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, failingDefThunk) @>
            <| fun e -> <@ e.Message.StartsWith "Defaulting Failed" && e.Message.Contains "--working-directory" @>

    [<Fact>]
    let ``Trap defaulting function exceptions (for overloads with parse functions)`` () =
        let parser (_ : string): string = failwith "should not be triggered"
        let failingDefThunk (): string = failwith "Defaulting Failed"
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, failingDefThunk, parser, showUsage = false) @>
            <| fun e -> <@ e.Message = "Defaulting Failed" && e.ErrorCode = ErrorCode.PostProcess @>
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, failingDefThunk, parser)  @>
            <| fun e -> <@ e.Message.StartsWith "Defaulting Failed" && e.Message.Contains "--working-directory" @>

    [<Fact>]
    let ``Trap post processing exceptions for GetResult overloads with defaulting functions`` () =
        let parser value: string = if value = "default" then failwith "Parse Failed" else failwith "unexpected"
        let okDefThunk (): string = "default"
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, okDefThunk, parser, showUsage = false) @>
            <| fun e -> <@ e.Message = "Parse Failed" && e.ErrorCode = ErrorCode.PostProcess @>
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, okDefThunk, parser)  @>
            <| fun e -> <@ e.Message.StartsWith "Parse Failed" && e.Message.Contains "--working-directory" @>

    [<Fact>]
    let ``Trap post processing exceptions for GetResult overloads with default values`` () =
        let def: string = "default"
        let parser value: string = if value = "default" then failwith "Parse Failed" else failwith "unexpected"
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, def, parser, showUsage = false) @>
            <| fun e -> <@ e.Message = "Parse Failed" && e.ErrorCode = ErrorCode.PostProcess @>
        raisesWith<ArguParseException>
            <@ results.GetResult(Working_Directory, def, parser)  @>
            <| fun e -> <@ e.Message.StartsWith "Parse Failed" && e.Message.Contains "--working-directory" @>
