namespace Argu.Tests

#nowarn "44" // Legacy *Assignment attributes are obsolete; tests deliberately use them.

open Xunit
open Swensen.Unquote

open Argu

/// Tests for the unified [<Assignment>] attribute (PR 27).
module ``Argu Tests AssignmentAttribute`` =

    // --- Unified [<Assignment>] forms ---

    type EqualsViaNewAttr =
        | [<Assignment("=")>] Port of int
        interface IArgParserTemplate with member this.Usage = "x"

    type ColonSpacedViaNewAttr =
        | [<Assignment(":", allowSpaced = true)>] Tag of string
        interface IArgParserTemplate with member this.Usage = "x"

    // --- Legacy attributes for parity comparison ---

    type EqualsViaLegacy =
        | [<EqualsAssignment>] Port of int
        interface IArgParserTemplate with member this.Usage = "x"

    type ColonSpacedViaLegacy =
        | [<ColonAssignmentOrSpaced>] Tag of string
        interface IArgParserTemplate with member this.Usage = "x"

    // --- Equals tests ---

    [<Fact>]
    let ``[<Assignment("=")>] parses --port=8080`` () =
        let parser = ArgumentParser.Create<EqualsViaNewAttr>(programName = "app")
        let r = parser.ParseCommandLine([| "--port=8080" |], raiseOnUsage = false)
        test <@ r.GetResult(EqualsViaNewAttr.Port) = 8080 @>

    [<Fact>]
    let ``[<Assignment("=")>] rejects --port 8080 (no spaced form)`` () =
        let parser = ArgumentParser.Create<EqualsViaNewAttr>(programName = "app")
        let raised =
            try parser.ParseCommandLine([| "--port"; "8080" |], raiseOnUsage = false) |> ignore ; false
            with :? ArguParseException -> true
        test <@ raised @>

    [<Fact>]
    let ``[<Assignment("=")>] matches [<EqualsAssignment>] parity`` () =
        let novel =
            ArgumentParser.Create<EqualsViaNewAttr>(programName = "app")
                .ParseCommandLine([| "--port=42" |], raiseOnUsage = false)
                .GetResult(EqualsViaNewAttr.Port)
        let legacy =
            ArgumentParser.Create<EqualsViaLegacy>(programName = "app")
                .ParseCommandLine([| "--port=42" |], raiseOnUsage = false)
                .GetResult(EqualsViaLegacy.Port)
        test <@ novel = legacy @>

    // --- Colon-or-spaced tests ---

    [<Fact>]
    let ``[<Assignment(":", allowSpaced = true)>] accepts --tag:value`` () =
        let parser = ArgumentParser.Create<ColonSpacedViaNewAttr>(programName = "app")
        let r = parser.ParseCommandLine([| "--tag:hello" |], raiseOnUsage = false)
        test <@ r.GetResult(ColonSpacedViaNewAttr.Tag) = "hello" @>

    [<Fact>]
    let ``[<Assignment(":", allowSpaced = true)>] accepts --tag spaced`` () =
        let parser = ArgumentParser.Create<ColonSpacedViaNewAttr>(programName = "app")
        let r = parser.ParseCommandLine([| "--tag"; "hello" |], raiseOnUsage = false)
        test <@ r.GetResult(ColonSpacedViaNewAttr.Tag) = "hello" @>

    [<Fact>]
    let ``[<Assignment(":", allowSpaced = true)>] matches [<ColonAssignmentOrSpaced>] parity`` () =
        let novel =
            ArgumentParser.Create<ColonSpacedViaNewAttr>(programName = "app")
                .ParseCommandLine([| "--tag"; "v" |], raiseOnUsage = false)
                .GetResult(ColonSpacedViaNewAttr.Tag)
        let legacy =
            ArgumentParser.Create<ColonSpacedViaLegacy>(programName = "app")
                .ParseCommandLine([| "--tag"; "v" |], raiseOnUsage = false)
                .GetResult(ColonSpacedViaLegacy.Tag)
        test <@ novel = legacy @>

    // --- Conflict detection ---

    type ConflictingAttrs =
        | [<Assignment("=")>] [<CustomAssignment("=")>] Port of int
        interface IArgParserTemplate with member this.Usage = "x"

    [<Fact>]
    let ``Mixing [<Assignment>] with legacy CustomAssignment raises a clear error`` () =
        let raised =
            try
                ArgumentParser.Create<ConflictingAttrs>(programName = "app") |> ignore
                None
            with :? ArguException as e -> Some e.Message
        match raised with
        | None -> failwith "expected ArguException for mixed Assignment + CustomAssignment"
        | Some msg ->
            test <@ msg.Contains "Assignment" @>
