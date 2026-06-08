module Argu.Tests.EnvVarPrefixTests

open Swensen.Unquote
open System
open Xunit

open Argu

type Args =
    | [<CustomAppSettings("HOST")>] HostName of string
    | [<CustomAppSettings("PORT")>] Port of int
    interface IArgParserTemplate with member this.Usage = "x"

/// Sets an environment variable and restores its prior value on Dispose,
/// so a test can scope the override with `use _ = envOverride key value`.
let private envOverride (key : string) (value : string) : IDisposable =
    let prior = Environment.GetEnvironmentVariable key
    Environment.SetEnvironmentVariable(key, value)
    { new IDisposable with member _.Dispose() = Environment.SetEnvironmentVariable(key, prior) }

[<Fact>]
let ``FromEnvironmentVariables(prefix) prepends prefix to key`` () =
    let reader = ConfigurationReader.FromEnvironmentVariables(prefix = "MYAPP_")
    use _ = envOverride "MYAPP_HOST" "example.com"
    test <@ reader.GetValue("HOST") = "example.com" @>

[<Fact>]
let ``FromEnvironmentVariables(prefix) returns null for missing keys`` () =
    let reader = ConfigurationReader.FromEnvironmentVariables(prefix = "ARGU_TEST_MISSING_")
    // Ensure no stray env var; SetEnvironmentVariable(null) clears it.
    Environment.SetEnvironmentVariable("ARGU_TEST_MISSING_NOPE", null)
    test <@ reader.GetValue("NOPE") = null @>

[<Fact>]
let ``FromEnvironmentVariables(prefix) parses round-trip through ArgumentParser`` () =
    use _ = envOverride "DEMO_HOST" "host.local"
    use _ = envOverride "DEMO_PORT" "443"
    let parser = ArgumentParser.Create<Args>(programName = "demo")
    let reader = ConfigurationReader.FromEnvironmentVariables(prefix = "DEMO_")
    let results = parser.ParseConfiguration(reader, ignoreMissing = true)
    test <@ results.GetResult(HostName) = "host.local" @>
    test <@ results.GetResult(Port) = 443 @>

[<Fact>]
let ``No-arg FromEnvironmentVariables still works without prefix`` () =
    // Sanity check: the legacy zero-arg overload is preserved.
    use _ = envOverride "ARGU_TEST_PLAIN" "value"
    let reader = ConfigurationReader.FromEnvironmentVariables()
    test <@ reader.GetValue("ARGU_TEST_PLAIN") = "value" @>
