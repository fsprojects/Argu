namespace Argu

open System
open System.IO
open System.Configuration
open System.Collections.Generic
open System.Reflection
open System.Threading.Tasks

/// Abstract key/value configuration reader
type IConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// Gets value corresponding to supplied key
    abstract GetValue : key:string -> string | null

/// <summary>
///     Asynchronous, batched flavour of <see cref="IConfigurationReader"/>.
///     Use when the underlying source is genuinely async (remote config
///     server, secrets vault, key/value store).
/// </summary>
/// <remarks>
///     The interface is batched by design: <see cref="GetValuesAsync"/>
///     receives every schema-derived key in a single call so a network
///     source can satisfy the whole parse with one round-trip.
///     Implementations should return only keys that were found; missing
///     keys must be absent from the returned dictionary (matching the
///     <c>null</c> contract of <see cref="IConfigurationReader.GetValue"/>).
///     A faulted <see cref="Task"/> propagates to the caller as a fatal
///     startup error; wrap with <see cref="ConfigurationReader.WithFallbackToNull"/>
///     for a best-effort policy that treats source unavailability as
///     "all keys missing".
/// </remarks>
type IAsyncConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// <summary>
    ///     Asynchronously fetches values for the supplied keys.
    ///     Implementations should return a dictionary containing only the
    ///     keys that were found; absent keys are treated as missing
    ///     (equivalent to <c>null</c> from
    ///     <see cref="IConfigurationReader.GetValue"/>).
    /// </summary>
    abstract GetValuesAsync : keys:IReadOnlyCollection<string> -> Task<IReadOnlyDictionary<string, string>>

/// Configuration reader that never returns a value
type NullConfigurationReader() =
    interface IConfigurationReader with
        member x.Name = "Null Configuration Reader"
        member x.GetValue _ = null

/// Environment variable-based configuration reader
type EnvironmentVariableConfigurationReader() =
    // order of environment variable target lookup
    let targets =
        [| EnvironmentVariableTarget.Process
           EnvironmentVariableTarget.User
           EnvironmentVariableTarget.Machine |]

    interface IConfigurationReader with
        member x.Name = "Environment Variables Configuration Reader"
        member x.GetValue(key:string) =
            let folder curr (target : EnvironmentVariableTarget) =
                match curr with
                | null -> Environment.GetEnvironmentVariable(key, target)
                | value -> value

            targets |> Array.fold folder null

/// Configuration reader dictionary proxy
type DictionaryConfigurationReader (keyValueDictionary : IDictionary<string, string>, ?name : string) =
    let name = defaultArg name "Dictionary configuration reader."
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key:string) =
            let ok,value = keyValueDictionary.TryGetValue key
            if ok then value else null

/// Function configuration reader proxy
type FunctionConfigurationReader (configFunc : string -> string option, ?name : string) =
    let name = defaultArg name "Function configuration reader."
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key:string) =
            match configFunc key with
            | None -> null
            | Some v -> v

/// AppSettings XML configuration reader
type AppSettingsConfigurationReader () =
    interface IConfigurationReader with
        member _.Name = "AppSettings configuration reader"
        member _.GetValue(key:string) = ConfigurationManager.AppSettings[key]

/// AppSettings XML configuration reader
type AppSettingsConfigurationFileReader private (xmlPath : string, kv : KeyValueConfigurationCollection) =
    member _.Path = xmlPath
    interface IConfigurationReader with
        member _.Name = sprintf "App.config configuration reader: %s" xmlPath
        member _.GetValue(key:string) =
            match kv[key] with
            | null -> null
            | entry -> entry.Value

    /// Create used supplied XML file path
    static member Create(path : string) =
        if not <| File.Exists path then raise <| FileNotFoundException(path)
        let fileMap = ExeConfigurationFileMap()
        fileMap.ExeConfigFilename <- path
        let config = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None)
        AppSettingsConfigurationFileReader(path, config.AppSettings.Settings)

/// Configuration reader implementations
type ConfigurationReader =
    /// Create a configuration reader that always returns null
    static member NullReader = NullConfigurationReader() :> IConfigurationReader

    /// Create a configuration reader instance using an IDictionary instance
    static member FromDictionary(keyValueDictionary : IDictionary<string,string>, ?name : string) =
        DictionaryConfigurationReader(keyValueDictionary, ?name = name) :> IConfigurationReader

    /// Create a configuration reader instance using an F# function
    static member FromFunction(reader : string -> string option, ?name : string) =
        FunctionConfigurationReader(reader, ?name = name) :> IConfigurationReader

    /// Create a configuration reader instance using environment variables
    static member FromEnvironmentVariables() =
        EnvironmentVariableConfigurationReader() :> IConfigurationReader

    /// Create a configuration reader instance using the application's resident AppSettings configuration
    static member FromAppSettings() = AppSettingsConfigurationReader() :> IConfigurationReader
    /// Create a configuration reader instance using a local xml App.Config file
    static member FromAppSettingsFile(path : string) = AppSettingsConfigurationFileReader.Create(path) :> IConfigurationReader
    /// Create a configuration reader instance using the location of an assembly file
    static member FromAppSettings(assembly : Assembly) =
        let path = assembly.Location
        if String.IsNullOrEmpty path then
            sprintf "Assembly location for '%O' is null or empty." assembly.Location
            |> invalidArg assembly.FullName

        AppSettingsConfigurationFileReader.Create(path + ".config") :> IConfigurationReader

    /// <summary>
    ///     Wraps a synchronous <see cref="IConfigurationReader"/> as an
    ///     <see cref="IAsyncConfigurationReader"/>. Each batch issues one
    ///     synchronous lookup per key against the wrapped reader; no real
    ///     batching is possible from a per-key source, but the async
    ///     contract is satisfied via <see cref="Task.FromResult"/>.
    /// </summary>
    static member AsAsync(reader : IConfigurationReader) : IAsyncConfigurationReader =
        { new IAsyncConfigurationReader with
            member _.Name = reader.Name
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) =
                let dict = Dictionary<string, string>(keys.Count)
                for k in keys do
                    match reader.GetValue k with
                    | null -> ()
                    | v -> dict[k] <- v
                Task.FromResult(dict :> IReadOnlyDictionary<string, string>) }

    /// <summary>
    ///     Bridges a per-key async source into an
    ///     <see cref="IAsyncConfigurationReader"/>. The function is invoked
    ///     once per requested key, sequentially. If the underlying source
    ///     supports batched retrieval natively, implement
    ///     <see cref="IAsyncConfigurationReader"/> directly to collapse the
    ///     N round-trips into one.
    /// </summary>
    static member FromAsyncFunction(reader : string -> Async<string option>, ?name : string) : IAsyncConfigurationReader =
        let name = defaultArg name "Async function configuration reader."
        { new IAsyncConfigurationReader with
            member _.Name = name
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) =
                task {
                    let dict = Dictionary<string, string>(keys.Count)
                    for k in keys do
                        let! v = reader k
                        match v with
                        | None -> ()
                        | Some v -> dict[k] <- v
                    return dict :> IReadOnlyDictionary<string, string>
                } }

    /// <summary>
    ///     Wraps an <see cref="IAsyncConfigurationReader"/> so transport
    ///     errors (faulted <see cref="Task"/>) are downgraded to "all keys
    ///     missing" rather than failing the parse. The optional
    ///     <paramref name="onFault"/> hook lets callers log to stderr or
    ///     telemetry; the default writes a single line to stderr.
    ///     Use when configuration is genuinely optional and the program
    ///     should still start with CLI defaults if the source is
    ///     unreachable.
    /// </summary>
    static member WithFallbackToNull(reader : IAsyncConfigurationReader, ?onFault : exn -> unit) : IAsyncConfigurationReader =
        let onFault = defaultArg onFault (fun ex -> eprintfn "[%s] unavailable: %s" reader.Name ex.Message)
        let empty = Dictionary<string, string>() :> IReadOnlyDictionary<string, string>
        { new IAsyncConfigurationReader with
            member _.Name = reader.Name + " (with null fallback)"
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) =
                task {
                    try
                        return! reader.GetValuesAsync keys
                    with ex ->
                        onFault ex
                        return empty
                } }