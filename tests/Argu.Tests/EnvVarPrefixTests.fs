namespace Argu.Tests

open System
open Xunit
open Swensen.Unquote

open Argu

/// Tests for the env-var prefix configuration reader (PR 20).
module ``Argu Tests EnvVarPrefix`` =

    type Args =
        | [<CustomAppSettings("HOST")>] HostName of string
        | [<CustomAppSettings("PORT")>] Port of int
        interface IArgParserTemplate with member this.Usage = "x"

    let private withEnv (key : string) (value : string) (body : unit -> 'T) : 'T =
        let prior = Environment.GetEnvironmentVariable key
        try
            Environment.SetEnvironmentVariable(key, value)
            body ()
        finally
            Environment.SetEnvironmentVariable(key, prior)

    [<Fact>]
    let ``FromEnvironmentVariables(prefix) prepends prefix to key`` () =
        let reader = ConfigurationReader.FromEnvironmentVariables(prefix = "MYAPP_")
        withEnv "MYAPP_HOST" "example.com" (fun () ->
            test <@ reader.GetValue("HOST") = "example.com" @>)

    [<Fact>]
    let ``FromEnvironmentVariables(prefix) returns null for missing keys`` () =
        let reader = ConfigurationReader.FromEnvironmentVariables(prefix = "ARGU_TEST_MISSING_")
        // Ensure no stray env var; SetEnvironmentVariable(null) clears it.
        Environment.SetEnvironmentVariable("ARGU_TEST_MISSING_NOPE", null)
        test <@ reader.GetValue("NOPE") = null @>

    [<Fact>]
    let ``FromEnvironmentVariables(prefix) parses round-trip through ArgumentParser`` () =
        withEnv "DEMO_HOST" "host.local" (fun () ->
            withEnv "DEMO_PORT" "443" (fun () ->
                let parser = ArgumentParser.Create<Args>(programName = "demo")
                let reader = ConfigurationReader.FromEnvironmentVariables(prefix = "DEMO_")
                let results = parser.ParseConfiguration(reader, ignoreMissing = true)
                test <@ results.GetResult(HostName) = "host.local" @>
                test <@ results.GetResult(Port) = 443 @>))

    [<Fact>]
    let ``No-arg FromEnvironmentVariables still works without prefix`` () =
        // Sanity check: the legacy zero-arg overload is preserved.
        withEnv "ARGU_TEST_PLAIN" "value" (fun () ->
            let reader = ConfigurationReader.FromEnvironmentVariables()
            test <@ reader.GetValue("ARGU_TEST_PLAIN") = "value" @>)
