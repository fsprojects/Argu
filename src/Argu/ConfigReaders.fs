namespace Argu

open System
open System.Configuration
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading.Tasks

/// Abstract key/value configuration reader
type IConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// Gets value corresponding to supplied key
    abstract GetValue : key : string -> string | null

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
///     startup error; wrap with <see cref="ConfigurationReader.WithFallback"/>
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
    abstract GetValuesAsync : keys : IReadOnlyCollection<string> -> Task<IReadOnlyDictionary<string, string>>

/// Configuration reader that never returns a value
type NullConfigurationReader () =
    interface IConfigurationReader with
        member _.Name = "Null Configuration Reader"
        member _.GetValue _ = null

/// Environment variable-based configuration reader
type EnvironmentVariableConfigurationReader () =
    // order of environment variable target lookup
    let targets =
        [| EnvironmentVariableTarget.Process
           EnvironmentVariableTarget.User
           EnvironmentVariableTarget.Machine |]

    interface IConfigurationReader with
        member _.Name = "Environment Variables Configuration Reader"
        member _.GetValue(key : string) =
            // NOTE this logic varies from a targetless Get in that it will pick up changes that a given process tree
            // has yet to propagate. A fresh shell should include machine and user level vars in the process env from the off
            let folder curr (target : EnvironmentVariableTarget) =
                match curr with
                | null -> Environment.GetEnvironmentVariable(key, target)
                | value -> value
            (null, targets) ||> Array.fold folder

/// Configuration reader dictionary proxy
type DictionaryConfigurationReader (values: IReadOnlyDictionary<string, string>, ?name : string) =
    let name = defaultArg name "Dictionary configuration reader."
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key : string) =
            let ok,value = values.TryGetValue key
            if ok then value else null

/// Function configuration reader proxy
type FunctionConfigurationReader (configFunc : string -> string option, ?name : string) =
    let name = defaultArg name "Function configuration reader."
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key : string) = configFunc key |> Option.toObj

/// AppSettings XML configuration reader
type AppSettingsConfigurationReader () =
    interface IConfigurationReader with
        member _.Name = "AppSettings configuration reader"
        member _.GetValue(key : string) = ConfigurationManager.AppSettings[key]

/// AppSettings XML configuration reader
type AppSettingsConfigurationFileReader private (xmlPath : string, kv : KeyValueConfigurationCollection) =
    member _.Path = xmlPath
    interface IConfigurationReader with
        member _.Name = $"App.config configuration reader: %s{xmlPath}"
        member _.GetValue(key : string) =
            match kv[key] with
            | null -> null
            | entry -> entry.Value

    /// Create used supplied XML file path
    static member Create(path : string) =
        if not <| File.Exists path then raise <| FileNotFoundException(path)
        let fileMap = ExeConfigurationFileMap(ExeConfigFilename = path)
        let config = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None)
        AppSettingsConfigurationFileReader(path, config.AppSettings.Settings)

/// Configuration reader implementations
[<AbstractClass; Sealed>]
type ConfigurationReader =

    /// Create a configuration reader that always returns null
    static member NullReader : IConfigurationReader = NullConfigurationReader()

    /// Create a configuration reader instance using an IDictionary instance
    static member FromDictionary(values: IReadOnlyDictionary<string,string>, ?name : string) : IConfigurationReader =
        DictionaryConfigurationReader(values, ?name = name)

    /// Create a configuration reader instance using an F# function
    static member FromFunction(reader : string -> string option, ?name : string) : IConfigurationReader =
        FunctionConfigurationReader(reader, ?name = name)

    /// Create a configuration reader instance using environment variables.
    /// Reads Process variables, falling back to latest User or latest Machine targets if not found.
    static member FromEnvironmentVariables() : IConfigurationReader = EnvironmentVariableConfigurationReader()

    /// <summary>
    /// Create a configuration reader that reads environment variables with a fixed prefix prepended to the requested key.<br/>
    /// Maps <c>--foo-bar</c> arguments onto <c>MYAPP_FOO_BAR</c> environment variables.<br/>
    /// As per <c>FromEnvironmentVariables()</c>, reads Process variables, falling back to latest User or latest Machine targets if not found.
    /// </summary>
    /// <param name="prefix">String prepended to each requested key (e.g. <c>"MYAPP_"</c>).</param>
    static member FromEnvironmentVariables(prefix : string) : IConfigurationReader =
        if isNull prefix then nullArg (nameof prefix)
        let inner = EnvironmentVariableConfigurationReader() :> IConfigurationReader
        let read (key : string) = inner.GetValue(prefix + key) |> Option.ofObj
        FunctionConfigurationReader(read, name = $"Environment Variables (prefix=%s{prefix})")

    /// Create a configuration reader instance using the application's resident AppSettings configuration
    static member FromAppSettings() : IConfigurationReader = AppSettingsConfigurationReader()

    /// Create a configuration reader instance using a local xml App.Config file
    static member FromAppSettingsFile(path : string) : IConfigurationReader = AppSettingsConfigurationFileReader.Create path

    /// Create a configuration reader instance using the location of an assembly file
    static member FromAppSettings(assembly : Assembly) : IConfigurationReader =
        let path = assembly.Location
        if String.IsNullOrEmpty path then
            invalidArg assembly.FullName $"Assembly location for '{assembly.Location}' is null or empty."
        AppSettingsConfigurationFileReader.Create(path + ".config")

    /// <summary>
    /// Wraps a synchronous <see cref="IConfigurationReader"/> as an <see cref="IAsyncConfigurationReader"/>.<br/>
    /// Each batch issues one synchronous lookup per key against the wrapped reader;
    /// no real batching is possible from a per-key source, but the async contract is satisfied via <see cref="Task.FromResult"/>.
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
                Task.FromResult(dict) }

    /// <summary>
    /// Bridges a per-key async source into an <see cref="IAsyncConfigurationReader"/>.<br/>
    /// The function is invoked once per requested key, sequentially.<br/>
    /// If the underlying source supports batched retrieval natively,
    /// implement <see cref="IAsyncConfigurationReader"/> directly to collapse the N round-trips into one.
    /// </summary>
    static member FromFunctionAsync(reader : string -> Task<string option>, ?name : string) : IAsyncConfigurationReader =
        let name = defaultArg name "Async function configuration reader."
        { new IAsyncConfigurationReader with
            member _.Name = name
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) = task {
                let dict = Dictionary<string, string>(keys.Count)
                for k in keys do
                    match! reader k with
                    | None -> ()
                    | Some v -> dict[k] <- v
                return dict } }

    /// <summary>
    /// Wraps an <see cref="IAsyncConfigurationReader"/> so errors (faulted <see cref="Task"/>) are substituted
    /// by a substitute configuration rather than failing the parse.
    /// </summary>
    /// <param name="reader">The Async reader to be wrapped.</param>
    /// <param name="fallback">Callback that sees the triggering exception, facilitating logging to stderr or telemetry.</param>
    static member WithFallback(reader : IAsyncConfigurationReader, fallback: exn -> Task<IReadOnlyDictionary<string, string>>) : IAsyncConfigurationReader =
        { new IAsyncConfigurationReader with
            member _.Name = reader.Name + " (with null fallback)"
            member _.GetValuesAsync(keys : IReadOnlyCollection<string>) = task {
                try return! reader.GetValuesAsync keys
                with ex -> return! fallback ex } }
