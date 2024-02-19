(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../src/Argu/bin/Release/netstandard2.0"
#r "Argu.dll"

open Argu

type Args =
    | Working_Directory of path: string
    | Listener of host: string * port: int
    | Log_Level of level: int
    | Detach
with
    interface IArgParserTemplate with
        member _.Usage =
            "(see the Tutorial for how this should be written)"

(**

# Introduction

Argu (pronounced "Argue") is a declarative CLI argument parser for F# console applications.
It allows modelling the command-line syntax using discriminated unions,
which the library converts into a working parser using reflection.

Argu is a mature library that comes with many features

  * Declarative: easily model your syntax definitions using F# unions.
  * Convenient: automatic derivation of CLI syntax and documentation.
  * Customizable: control most aspects of your parser behaviour.
  * Subcommands: use contextual syntax with nested argument schemata.

It can be installed using <a href="https://nuget.org/packages/Argu">NuGet</a>.

## Basic Concepts

The library is based on the simple observation that
configuration parameters can be naturally described using discriminated unions.
For instance:

*)

type Arguments =
    | Working_Directory of path: string
    | Listener of host: string * port: int
    | Log_Level of level: int
    | Detach

(**

Argu takes such discriminated unions and generates
a corresponding argument parsing scheme.
For example, a parser generated from the above template would
take the following command line input

    [lang=bash]
    --working-directory /var/run --listener localhost 8080 --detach

and parse it into the list
*)

[ Working_Directory "/var/run"; Listener("localhost", 8080); Detach ]

(**

Argu is also capable of reading the `AppSettings` section
of an application's configuration file:

    [lang=xml]
    <appSettings>
        <add key="working directory" value="C:\temp" />
        <add key="listener" value="192.168.0.3, 2675" />
        <add key="log level" value="3" />
        <add key="detach" value="true" />
    </appSettings>

Furthermore, you can parse environment variables, by supplying the an `EnvironmentVariableReader` to the `Parse` call:
*)

let argv = [| "--log-level"; "3" |]
let reader = EnvironmentVariableConfigurationReader() :> IConfigurationReader
let parser =  ArgumentParser.Create<Args>(programName = "rutta")
// pass the reader to the Parse call
let results = parser.Parse(argv, configurationReader = reader)

(**
## Who uses Argu?

  * [MBrace](http://m-brace.net/)

  * [FAKE](http://fsharp.github.io/FAKE/)

  * [Paket](http://fsprojects.github.io/Paket/)

  * [Logary](https://logary.tech)

  * [Equinox + Propulsion](https://github.com/jet/Equinox)

## Documentation

  * [Tutorial](tutorial.html) A short walkthrough of Argu features.

  * [API Reference](reference/index.html) contains automatically generated documentation for all types,
    modules and functions in the library.

## Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork
the project and submit pull requests.

The library is available under the MIT License.
For more information see the [License file][license] in the GitHub repository.

  [gh]: https://github.com/fsprojects/Argu
  [issues]: https://github.com/fsprojects/Argu/issues
  [license]: https://github.com/fsprojects/Argu/blob/master/LICENSE.md

*)
