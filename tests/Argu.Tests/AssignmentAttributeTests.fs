module Argu.Tests.AssignmentAttributeTests

open Swensen.Unquote
open Xunit

open Argu

// --- Unified [<Assignment>] forms ---

type EqualsViaNewAttr =
    | [<Assignment("=")>] Port of int
    interface IArgParserTemplate with member this.Usage = "x"

type ColonSpacedViaNewAttr =
    | [<Assignment(":", allowSpaced = true)>] Tag of string
    interface IArgParserTemplate with member this.Usage = "x"

// --- Legacy attributes for parity comparison ---

#nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
type EqualsViaLegacy =
    | [<EqualsAssignment>] Port of int
    interface IArgParserTemplate with member this.Usage = "x"

type ColonSpacedViaLegacy =
    | [<ColonAssignmentOrSpaced>] Tag of string
    interface IArgParserTemplate with member this.Usage = "x"
#warnon 44

// --- Equals tests ---

let run<'T when 'T :> IArgParserTemplate> args = ArgumentParser.Create<'T>().ParseCommandLine args

[<Fact>]
let ``[<Assignment("=")>] parses --port=8080`` () =
    let r = run<EqualsViaNewAttr> [| "--port=8080" |]
    test <@ r.GetResult EqualsViaNewAttr.Port = 8080 @>

[<Fact>]
let ``[<Assignment("=")>] rejects --port 8080 (no spaced form)`` () =
    raises<ArguParseException>
        <@ run<EqualsViaNewAttr>([| "--port"; "8080" |]) @>

[<Fact>]
let ``[<Assignment("=")>] matches [<EqualsAssignment>] parity`` () =
    let novel = run<EqualsViaNewAttr> [| "--port=42" |] |> _.GetResult(EqualsViaNewAttr.Port)
    let legacy = run<EqualsViaLegacy> [| "--port=42" |] |> _.GetResult(EqualsViaLegacy.Port)
    test <@ novel = legacy @>

// --- Colon-or-spaced tests ---

[<Fact>]
let ``[<Assignment(":", allowSpaced = true)>] accepts --tag:value`` () =
    let r = run<ColonSpacedViaNewAttr> [| "--tag:hello" |]
    test <@ r.GetResult ColonSpacedViaNewAttr.Tag = "hello" @>

[<Fact>]
let ``[<Assignment(":", allowSpaced = true)>] accepts --tag spaced`` () =
    let r = run<ColonSpacedViaNewAttr> [| "--tag"; "hello" |]
    test <@ r.GetResult ColonSpacedViaNewAttr.Tag = "hello" @>

[<Fact>]
let ``[<Assignment(":", allowSpaced = true)>] matches [<ColonAssignmentOrSpaced>] parity`` () =
    let novel = run<ColonSpacedViaNewAttr> [| "--tag"; "v" |] |> _.GetResult(ColonSpacedViaNewAttr.Tag)
    let legacy = run<ColonSpacedViaLegacy> [| "--tag"; "v" |] |> _.GetResult(ColonSpacedViaLegacy.Tag)
    test <@ novel = legacy @>

// --- Conflict detection ---

#nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
type ConflictingAttrs =
    | [<Assignment("=")>] [<CustomAssignment("=")>] Port of int
    interface IArgParserTemplate with member this.Usage = "x"
#warnon 44

[<Fact>]
let ``Mixing [<Assignment>] with legacy CustomAssignment raises a clear error`` () =
    raisesWith<ArguException>
        <@ ArgumentParser.Create<ConflictingAttrs>() @>
        (fun e -> <@ e.Message.Contains "mixes the 'Assignment' attribute" @>)

// --- Legacy tests prior to introduction of AssignmentAttribute ---

#nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
type DisallowedAssignmentArgs =
| [<EqualsAssignmentOrSpaced>] [<EqualsAssignment>] Flex_Equals_Assignment of string
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Flex_Equals_Assignment _ -> "Disallowed attribute combination"
#warnon 44

[<Fact>]
let ``Disallowed equals assignment combination throws`` () =
    raisesWith<ArguException> <@ ArgumentParser.Create<DisallowedAssignmentArgs>() @>

#nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.
type DisallowedArityWithAssignmentOrSpaced =
| [<EqualsAssignmentOrSpaced>] Flex_Equals_Assignment of string * int
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Flex_Equals_Assignment _ -> "Disallowed attribute / arity combination"
#warnon 44

[<Fact>]
let ``EqualsAssignmentOrSpaced and arity not one combination throws`` () =
    raisesWith<ArguException> <@ ArgumentParser.Create<DisallowedArityWithAssignmentOrSpaced>() @>
