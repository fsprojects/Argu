namespace Argu

open System
open System.Collections.Generic

/// Abstract key/value configuration reader
type IConfigurationReader =
    /// Configuration reader identifier
    abstract Name : string
    /// Gets value corresponding to supplied key
    abstract GetValue : key : string -> string | null

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
type DictionaryConfigurationReader (keyValueDictionary : IDictionary<string, string>, ?name : string) =
    let name = defaultArg name "Dictionary configuration reader."
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key : string) =
            let ok,value = keyValueDictionary.TryGetValue key
            if ok then value else null

/// Function configuration reader proxy
type FunctionConfigurationReader (configFunc : string -> string option, ?name : string) =
    let name = defaultArg name "Function configuration reader."
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key : string) = configFunc key |> Option.toObj

/// Configuration reader implementations
[<AbstractClass; Sealed>]
type ConfigurationReader =

    /// Create a configuration reader that always returns null
    static member NullReader : IConfigurationReader = NullConfigurationReader()

    /// Create a configuration reader instance using an IDictionary instance
    static member FromDictionary(keyValueDictionary : IDictionary<string,string>, ?name : string) : IConfigurationReader =
        DictionaryConfigurationReader(keyValueDictionary, ?name = name)

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

