namespace Argu

open System
open System.IO
open System.Collections.Generic

#if !CORE_CLR
open System.Configuration
open System.Reflection

// misc configuration reader implementations

type private AppSettingsConfigurationReader () =
    interface IConfigurationReader with
        member __.Name = "AppSettings configuration reader"
        member __.GetValue(key:string) = ConfigurationManager.AppSettings.[key]

type private AppSettingsConfigurationFileReader private (xmlPath : string, kv : KeyValueConfigurationCollection) =
    member __.Path = xmlPath
    interface IConfigurationReader with
        member __.Name = sprintf "App.config configuration reader: %s" xmlPath
        member __.GetValue(key:string) =
            match kv.[key] with
            | null -> null
            | entry -> entry.Value

    static member Create(path : string) =
        if not <| File.Exists path then raise <| new FileNotFoundException(path)
        let fileMap = new ExeConfigurationFileMap()
        fileMap.ExeConfigFilename <- path
        let config = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None)
        new AppSettingsConfigurationFileReader(path, config.AppSettings.Settings)

type private DictionaryConfigurationReader (keyValueDictionary : IDictionary<string, string>, ?name : string) =
    let name = defaultArg name "Dictionary configuration reader."
    interface IConfigurationReader with
        member __.Name = name
        member __.GetValue(key:string) =
            let ok,value = keyValueDictionary.TryGetValue key
            if ok then value else null

type private FunctionConfigurationReader (configFunc : string -> string option, ?name : string) =
    let name = defaultArg name "Function configuration reader."
    interface IConfigurationReader with
        member __.Name = name
        member __.GetValue(key:string) =
            match configFunc key with
            | None -> null
            | Some v -> v


/// Configuration reader implementations
type ConfigurationReader =
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

    /// Create a configuration reader instance using an IDictionary instance
    static member FromDictionary(keyValueDictionary : IDictionary<string,string>, ?name : string) =
        new DictionaryConfigurationReader(keyValueDictionary, ?name = name) :> IConfigurationReader

    /// Create a configuration reader instance using an F# function
    static member FromFunction(reader : string -> string option, ?name : string) =
        new FunctionConfigurationReader(reader, ?name = name) :> IConfigurationReader
        
    static member DefaultReader () = ConfigurationReader.FromAppSettings()
#else
/// Configuration reader implementations
type ConfigurationReader =
    static member DefaultReader () =
        { new IConfigurationReader with
            member x.Name = "Default - Empty Configuration Reader"
            member x.GetValue k = null }
#endif