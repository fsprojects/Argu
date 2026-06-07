namespace Argu.Extensions.Configuration

open Microsoft.Extensions.Configuration

open Argu

/// <summary>
///     Argu <see cref="IConfigurationReader"/> backed by a
///     <see cref="Microsoft.Extensions.Configuration.IConfiguration"/>.
///     The reader returns <c>null</c> when a key is missing, matching the
///     contract of every other Argu configuration reader.
/// </summary>
type MicrosoftExtensionsConfigurationReader(configuration : IConfiguration, ?name : string) =
    let name = defaultArg name "Microsoft.Extensions.Configuration reader"
    interface IConfigurationReader with
        member _.Name = name
        member _.GetValue(key : string) =
            // IConfiguration.[key] returns null for missing keys, which matches Argu's
            // expectation.
            configuration[key]

/// Factory helpers mirroring the shape of <see cref="ConfigurationReader"/>.
[<AbstractClass; Sealed>]
type ConfigurationReader =
    /// Create an Argu <see cref="IConfigurationReader"/> over an <see cref="IConfiguration"/>.
    static member FromMicrosoftConfiguration(configuration : IConfiguration, ?name : string) : IConfigurationReader =
        MicrosoftExtensionsConfigurationReader(configuration, ?name = name) :> _
