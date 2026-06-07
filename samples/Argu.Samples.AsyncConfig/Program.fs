module Argu.Samples.AsyncConfig.Program

open System
open System.Collections.Generic
open System.Threading.Tasks

open Argu

// -----------------------------------------------------------------------------
// CLI schema
//
// The schema mixes a few flavours of input so the sample exercises the full
// ParseAsync contract:
//   --vault-url      : required, taken from CLI only (--simulate path ignores it)
//   --db-host        : remote-overridable; AppSettings name "db-host"
//   --port           : remote-overridable; AppSettings name "port"
//   --feature-flag   : remote-overridable; AppSettings name "feature-flag"
//   --simulate       : run against an in-process fake reader instead of a real
//                      Azure Key Vault. Default true so the sample runs without
//                      Azure credentials.
// -----------------------------------------------------------------------------

type Args =
    | [<Unique; AltCommandLine("-v")>] Vault_Url of url:string
    | [<CustomAppSettings("db-host")>] Db_Host of host:string
    | [<CustomAppSettings("port")>] Port of port:int
    | [<CustomAppSettings("feature-flag")>] Feature_Flag of name:string
    | [<Unique>] Simulate of value:bool

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Vault_Url _ -> "Azure Key Vault URL (https://<name>.vault.azure.net/). Ignored under --simulate true."
            | Db_Host _ -> "DB hostname. Override the secret 'db-host'."
            | Port _ -> "TCP port. Override the secret 'port'."
            | Feature_Flag _ -> "Active feature flag. Override the secret 'feature-flag'."
            | Simulate _ -> "Use an in-process fake reader instead of Azure Key Vault. Defaults to true."


// -----------------------------------------------------------------------------
// Azure Key Vault reader
//
// The Azure SDK exposes GetSecretAsync per secret, no batched primitive. To
// honour Argu's "one round-trip per parse" intent we issue every requested
// secret in parallel via Task.WhenAll. For a vault with N relevant secrets
// this collapses to ceil(N / concurrencyLimit) Azure round-trips - effectively
// one when N is small.
// -----------------------------------------------------------------------------

module KeyVault =
    open Azure
    open Azure.Identity
    open Azure.Security.KeyVault.Secrets

    /// Build a SecretClient using DefaultAzureCredential (picks up Azure CLI
    /// login, managed identity, etc., in the documented chain).
    let createClient (vaultUrl : string) : SecretClient =
        SecretClient(Uri(vaultUrl), DefaultAzureCredential())

    /// IAsyncConfigurationReader backed by Azure Key Vault.
    ///
    /// Failure semantics:
    ///   - Vault unreachable / auth failure  -> the underlying Task faults
    ///     with RequestFailedException; the fault propagates out of
    ///     GetValuesAsync. Callers wrap with WithFallbackToNull if best-effort
    ///     is desired.
    ///   - Individual secret missing (404)   -> caught locally and treated as
    ///     "key not present" (absent from the returned dictionary), matching
    ///     the IConfigurationReader.GetValue null contract.
    let asReader (client : SecretClient) : IAsyncConfigurationReader =
        { new IAsyncConfigurationReader with
            member _.Name = sprintf "Azure Key Vault @ %O" client.VaultUri
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) =
                task {
                    let tasks =
                        keys
                        |> Seq.map (fun k ->
                            task {
                                try
                                    let! resp = client.GetSecretAsync(k)
                                    return Some (k, resp.Value.Value)
                                with :? RequestFailedException as ex when ex.Status = 404 ->
                                    return None
                            })
                        |> Seq.toArray
                    let! all = Task.WhenAll tasks
                    let dict = Dictionary<string, string>(all.Length)
                    for kv in all do
                        match kv with
                        | Some (k, v) -> dict[k] <- v
                        | None -> ()
                    return dict :> IReadOnlyDictionary<string, string>
                } }


// -----------------------------------------------------------------------------
// Simulated reader for offline runs
// -----------------------------------------------------------------------------

module Simulated =
    /// Fake "remote" reader. Demonstrates the batched contract without
    /// requiring Azure credentials. The 50ms delay stands in for network RTT
    /// and proves the call is genuinely async.
    let reader : IAsyncConfigurationReader =
        let seed =
            dict [
                "db-host",      "db.prod.internal"
                "port",         "5432"
                "feature-flag", "v2-routing"
            ]
        { new IAsyncConfigurationReader with
            member _.Name = "simulated-in-process-vault"
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) =
                task {
                    do! Task.Delay 50
                    let result = Dictionary<string, string>(keys.Count)
                    for k in keys do
                        match seed.TryGetValue k with
                        | true, v -> result[k] <- v
                        | _ -> ()
                    return result :> IReadOnlyDictionary<string, string>
                } }


// -----------------------------------------------------------------------------
// Entry point
//
// Demonstrates the three-flavour failure model:
//   1. ArguException at Create     -> schema is broken; fatal.
//   2. Faulted Task from reader    -> source unavailable; fatal unless wrapped
//                                      with WithFallbackToNull.
//   3. ArguParseException at Parse -> user input invalid; print usage + exit.
// -----------------------------------------------------------------------------

[<EntryPoint>]
let main argv =
    // (1) ArguException at construction: schema-level errors. We let this
    // propagate; in real apps a unit test for ArgumentParser<T>.CheckStructure()
    // ensures this never reaches production. See V7 design in fsprojects/Argu#326.
    let parser = ArgumentParser.Create<Args>(programName = "argu-async-config")

    // Pre-parse the CLI synchronously so we know whether to wire a real vault
    // reader or the simulated one. The actual parse-with-config runs below.
    let cli =
        try parser.Parse(argv, raiseOnUsage = true)
        with :? ArguParseException as ex ->
            // (3) ArguParseException: user gave us bad CLI input. Standard
            // friendly error path.
            eprintfn "%s" ex.Message
            exit 1

    let useSimulated = cli.GetResult(Simulate, defaultValue = true)

    let baseReader =
        if useSimulated then
            Simulated.reader
        else
            let url = cli.GetResult(Vault_Url)
            KeyVault.asReader (KeyVault.createClient url)

    // (2) Faulted Task from the reader: wrap with WithFallbackToNull so vault
    // unavailability degrades to "all keys missing" plus a stderr line.
    // Without this wrapper, the fault would propagate and abort startup,
    // which is also a legitimate choice for hard-required secrets.
    let reader =
        ConfigurationReader.WithFallbackToNull(
            baseReader,
            onFault = fun ex ->
                eprintfn "warning: %s unavailable; CLI defaults only (%s)"
                    baseReader.Name
                    ex.Message)

    let results =
        try
            parser.ParseAsync(argv, configurationReader = reader).Result
        with :? AggregateException as agg when (agg.InnerException :? ArguParseException) ->
            eprintfn "%s" agg.InnerException.Message
            exit 1

    printfn "Resolved configuration:"
    printfn "  source       = %s" reader.Name
    printfn "  db-host      = %s" (results.GetResult(Db_Host, defaultValue = "<missing>"))
    printfn "  port         = %s" (results.TryGetResult(Port) |> Option.map string |> Option.defaultValue "<missing>")
    printfn "  feature-flag = %s" (results.GetResult(Feature_Flag, defaultValue = "<missing>"))

    0
