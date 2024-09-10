namespace Argu

open System
open System.Collections.Generic

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

