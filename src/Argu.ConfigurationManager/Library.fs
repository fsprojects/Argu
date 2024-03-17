namespace Argu

open System.Configuration
open System.IO

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

[<AutoOpen>]
module ConfigurationReaderExtensions =
    open System.Reflection
    open System

    /// Configuration reader implementations
    type ConfigurationReader with

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
