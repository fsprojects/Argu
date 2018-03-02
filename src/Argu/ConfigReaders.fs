namespace Argu

open System
open System.IO
open System.Configuration
open System.Collections.Generic
open System.Reflection

/// Abstract key/value configuration reader
type IConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// Gets value corresponding to supplied key
    abstract GetValue : key:string -> string

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
        member __.Name = name
        member __.GetValue(key:string) =
            let ok,value = keyValueDictionary.TryGetValue key
            if ok then value else null

/// Function configuration reader proxy
type FunctionConfigurationReader (configFunc : string -> string option, ?name : string) =
    let name = defaultArg name "Function configuration reader."
    interface IConfigurationReader with
        member __.Name = name
        member __.GetValue(key:string) =
            match configFunc key with
            | None -> null
            | Some v -> v

/// AppSettings XML configuration reader
type AppSettingsConfigurationReader () =
    interface IConfigurationReader with
        member __.Name = "AppSettings configuration reader"
        member __.GetValue(key:string) = ConfigurationManager.AppSettings.[key]

/// AppSettings XML configuration reader
type AppSettingsConfigurationFileReader private (xmlPath : string, kv : KeyValueConfigurationCollection) =
    member __.Path = xmlPath
    interface IConfigurationReader with
        member __.Name = sprintf "App.config configuration reader: %s" xmlPath
        member __.GetValue(key:string) =
            match kv.[key] with
            | null -> null
            | entry -> entry.Value

    /// Create used supplied XML file path
    static member Create(path : string) =
        if not <| File.Exists path then raise <| new FileNotFoundException(path)
        let fileMap = new ExeConfigurationFileMap()
        fileMap.ExeConfigFilename <- path
        let config = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None)
        new AppSettingsConfigurationFileReader(path, config.AppSettings.Settings)

/// Configuration reader implementations
type ConfigurationReader =
    /// Create a configuration reader that always returns null
    static member NullReader = new NullConfigurationReader() :> IConfigurationReader

    /// Create a configuration reader instance using an IDictionary instance
    static member FromDictionary(keyValueDictionary : IDictionary<string,string>, ?name : string) =
        new DictionaryConfigurationReader(keyValueDictionary, ?name = name) :> IConfigurationReader

    /// Create a configuration reader instance using an F# function
    static member FromFunction(reader : string -> string option, ?name : string) =
        new FunctionConfigurationReader(reader, ?name = name) :> IConfigurationReader

    /// Create a configuration reader instance using environment variables
    static member FromEnvironmentVariables() =
        new EnvironmentVariableConfigurationReader() :> IConfigurationReader

    /// Create a configuration reader instance using the application's resident AppSettings configuration
    static member FromAppSettings() = new AppSettingsConfigurationReader() :> IConfigurationReader
    /// Create a configuration reader instance using a local xml App.Config file
    static member FromAppSettingsFile(path : string) = AppSettingsConfigurationFileReader.Create(path) :> IConfigurationReader
    /// Create a configuration reader instance using the location of an assembly file
    static member FromAppSettings(assembly : Assembly) =
        let path = assembly.Location
        if String.IsNullOrEmpty path then
            sprintf "Assembly location for '%O' is null or empty." assembly.Location
            |> invalidArg assembly.FullName

        AppSettingsConfigurationFileReader.Create(path + ".config") :> IConfigurationReader