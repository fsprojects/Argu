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

/// Asynchronous flavour of <see cref="IConfigurationReader"/>. Use when
/// the underlying source is genuinely async (remote config server,
/// secrets vault, etc.); a sync <see cref="IConfigurationReader"/> can
/// also be exposed through <see cref="ConfigurationReader.AsAsync"/>
/// when the parser already takes async readers.
type IAsyncConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// Asynchronously gets the value corresponding to the supplied key.
    /// Implementations should return <c>null</c> for missing keys (same
    /// contract as <see cref="IConfigurationReader.GetValue"/>).
    abstract GetValueAsync : key:string -> Task<string>

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
    ///     <see cref="IAsyncConfigurationReader"/>. <c>GetValueAsync</c>
    ///     returns a completed Task; useful for adapting existing readers
    ///     into an async pipeline.
    /// </summary>
    static member AsAsync(reader : IConfigurationReader) : IAsyncConfigurationReader =
        { new IAsyncConfigurationReader with
            member _.Name = reader.Name
            member _.GetValueAsync(key : string) = Task.FromResult(reader.GetValue key) }

    /// <summary>
    ///     Create an <see cref="IAsyncConfigurationReader"/> from an
    ///     F# async function. The function returns <c>None</c> for missing keys.
    /// </summary>
    static member FromAsyncFunction(reader : string -> Async<string option>, ?name : string) : IAsyncConfigurationReader =
        let name = defaultArg name "Async function configuration reader."
        { new IAsyncConfigurationReader with
            member _.Name = name
            member _.GetValueAsync(key : string) =
                task {
                    let! v = reader key
                    return
                        match v with
                        | None -> null
                        | Some v -> v
                } }
