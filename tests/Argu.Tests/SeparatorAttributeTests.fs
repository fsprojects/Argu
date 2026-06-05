module Argu.Tests.SeparatorAttributeTests

open Swensen.Unquote
open Xunit

open Argu

let run<'T when 'T :> IArgParserTemplate> argv = ArgumentParser.Create<'T>().ParseCommandLine argv

module SeparatorAttributeWithEquals =

    type EqualsViaNewAttr =
        | [<Separator("=")>] Port of int
        interface IArgParserTemplate with member this.Usage = ""

    #nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
    type EqualsViaLegacy =
        | [<EqualsAssignment>] Port of int
        interface IArgParserTemplate with member this.Usage = "x"
    #warnon 44

    [<Fact>]
    let ``[<Separator("=")>] parses --port=8080`` () =
        let r = run<EqualsViaNewAttr> [| "--port=8080" |]
        test <@ r.GetResult EqualsViaNewAttr.Port = 8080 @>

    [<Fact>]
    let ``[<Separator("=")>] rejects --port 8080 (no spaced form)`` () =
        raises<ArguParseException>
            <@ run<EqualsViaNewAttr>([| "--port"; "8080" |]) @>

    [<Fact>]
    let ``[<Separator("=")>] matches [<EqualsAssignment>] parity`` () =
        let novel = let r = run<EqualsViaNewAttr> [| "--port=42" |] in r.GetResult EqualsViaNewAttr.Port
        let legacy = let r = run<EqualsViaLegacy> [| "--port=42" |] in r.GetResult EqualsViaLegacy.Port
        test <@ novel = legacy @>

module ColonOrSpaced =

    type ColonSpacedViaNewAttr =
        | [<Separator(":", orSpace = true)>] Tag of string
        interface IArgParserTemplate with member this.Usage = ""

    #nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
    type ColonSpacedViaLegacy =
        | [<ColonAssignmentOrSpaced>] Tag of string
        interface IArgParserTemplate with member this.Usage = "x"
    #warnon 44

    [<Fact>]
    let ``[<Separator(":", orSpace = true)>] accepts --tag:value`` () =
        let r = run<ColonSpacedViaNewAttr> [| "--tag:hello" |]
        test <@ r.GetResult ColonSpacedViaNewAttr.Tag = "hello" @>

    [<Fact>]
    let ``[<Separator(":", orSpace = true)>] accepts --tag spaced`` () =
        let r = run<ColonSpacedViaNewAttr> [| "--tag"; "hello" |]
        test <@ r.GetResult ColonSpacedViaNewAttr.Tag = "hello" @>

    [<Fact>]
    let ``[<Separator(":", orSpace = true)>] matches [<ColonAssignmentOrSpaced>] parity`` () =
        let novel = let r = run<ColonSpacedViaNewAttr> [| "--tag"; "v" |] in r.GetResult ColonSpacedViaNewAttr.Tag
        let legacy = let r = run<ColonSpacedViaLegacy> [| "--tag"; "v" |] in r.GetResult ColonSpacedViaLegacy.Tag
        test <@ novel = legacy @>

module LegacyValidations =

    #nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
    type ConflictingAttrs =
        | [<Separator "=">] [<CustomAssignment "=">] Port of int
        interface IArgParserTemplate with member this.Usage = "x"
    type DisallowedAssignmentArgs =
        | [<EqualsAssignmentOrSpaced>] [<EqualsAssignment>] Flex_Equals_Assignment of string
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Flex_Equals_Assignment _ -> "Disallowed attribute combination"
    type DisallowedArityWithAssignmentOrSpaced =
        | [<EqualsAssignmentOrSpaced>] Flex_Equals_Assignment of string * int
        interface IArgParserTemplate with
            member a.Usage =
                match a with
                | Flex_Equals_Assignment _ -> "Disallowed attribute / arity combination"
    #warnon 44

    [<Fact>]
    let ``Mixing [<Separator>] with legacy CustomAssignment raises a clear error`` () =
        raisesWith<ArguException>
            <@ ArgumentParser.Create<ConflictingAttrs>() @>
            <| fun e -> <@ e.Message.Contains "has 'Separator' attribute, but also" @>

    [<Fact>]
    let ``Disallowed equals assignment combination throws`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<DisallowedAssignmentArgs>() @>

    [<Fact>]
    let ``EqualsAssignmentOrSpaced and arity not one combination throws`` () =
        raisesWith<ArguException> <@ ArgumentParser.Create<DisallowedArityWithAssignmentOrSpaced>() @>

// Moved from Tests.fs; TODO some duplication needs removing
module Assignment =

    open Argu.Tests.Main

    let run argv = parser.ParseCommandLine(argv, ignoreMissing = true)

    [<Fact>]
    let ``Should fail if assigment with Separator only has no separator.`` () =
        raisesWith<ArguParseException>
            <@ run [|"--assignment"; "value"|] @>
            <| fun e -> <@ e.FirstLine.Contains "missing an assignment" @>

    [<Fact>]
    let ``Parse colon assignment 1`` () =
        let res = run [| "--assignment:foobar" |]
        test <@ res.GetResult Assignment = "foobar" @>

    [<Fact>]
    let ``Parse colon assignment 2`` () =
        let arg = [ Assignment "foo bar" ]
        let res = parser.PrintCommandLineArguments arg |> run
        test <@ res.GetResult Assignment = "foo bar" @>

    [<Fact>]
    let ``Parse key-value equals assignment`` () =
        let arg = [ Env("foo", "bar") ]
        let res = parser.PrintCommandLineArguments arg |> run
        test <@ res.GetResult Env = ("foo", "bar") @>

    [<Fact>]
    let ``Parse key-value equals assignment 2`` () =
        let res = run [|"--env"; "foo==bar"|]
        test <@ res.GetResult Env = ("foo", "=bar") @>

    [<Fact>]
    let ``Parse equals assignment`` () =
        let res = run [|"--dir=../../my-relative-path"|]
        test <@ res.GetResult Dir = "../../my-relative-path" @>

    [<Fact>]
    let ``Parse equals assignment 2`` () =
        let res = run [|"--dir==foo"|]
        test <@ res.GetResult Dir = "=foo" @>

    [<Fact>]
    let ``Parse equals or space assignment with equals`` () =
        let res = run [|"--flex-equals-assignment=../../my-relative-path"; "--dir==foo"|]
        test <@ res.GetResult Flex_Equals_Assignment = "../../my-relative-path" @>

    [<Fact>]
    let ``Parse equals or space assignment with colon fails`` () =
        raises<ArguParseException> <@ run [|"--flex-equals-assignment:../../my-relative-path"; "--dir==foo"|] @>

    [<Fact>]
    let ``Parse equals or space assignment with space`` () =
        let res = run [|"--flex-equals-assignment"; "../../my-relative-path"; "--dir==foo"|]
        test <@ res.GetResult Flex_Equals_Assignment = "../../my-relative-path" @>

    [<Fact>]
    let ``Parse equals or space assignment with space and optional type`` () =
        let res = run [|"--flex-equals-assignment-with-option"; "../../my-relative-path"; "--dir==foo"|]
        test <@ res.GetResult Flex_Equals_Assignment_With_Option = Some "../../my-relative-path" @>

    [<Fact>]
    let ``Parse colon or space assignment with colon`` () =
        // No need to test space assignment or assignment failure, as EitherSpaceOrEqualsAssignmentAttribute and
        // EitherSpaceOrColonAssignmentAttribute share the same underlying implementation.
        let res = run [|"--flex-colon-assignment:../../my-relative-path"; "--dir==foo"|]
        test <@ res.GetResult Flex_Colon_Assignment = "../../my-relative-path" @>

    [<Fact>]
    let ``Should fail on incorrect assignment 1`` () =
        raises<ArguParseException> <@ run [|"--dir:foo"|] @>
