# UnionArgParser

A declarative argument parser for F# applications.

Get the Nuget package [here](http://www.nuget.org/packages/UnionArgParser/).

## Introduction

The library is based on the simple observation that 
configuration parameters can be naturally described using discriminated unions. 
For instance:
```fsharp
type Arguments =
    | Working_Directory of string
    | Listener of string * int // hostname * port
    | Log_Level of int
    | Detach
```
UnionArgParser takes such discriminated unions and generates 
a corresponding argument parsing scheme. 
For example, a parser generated from the above template would
take the following command line input
```bash
--working-directory /var/run --listener localhost 8080 --detach
```
and parse it into
```fsharp
[ Working_Directory "/var/run" ; Listener("localhost", 8080) ; Detach ]
```
UnionArgParser is also capable of reading the `AppSettings` section
of the application's configuration file:
```xml
<appSettings>
    <add key="working directory" value="C:\temp" />
    <add key="listener" value="192.168.0.3, 2675" />
    <add key="log level" value="3" />
    <add key="detach" value="true" />
</appSettings>
```
Both XML configuration and command line arguments can be parsed
at the same time. By default, command line parameters override
their corresponding XML configuration.

## Basic Usage

A minimal parser based on the above example can be created as follows:
```fsharp
type Arguments =
    | Working_Directory of string
    | Listener of string * int // hostname * port
    | Log_Level of int
    | Detach
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Listener _ -> "specify a listener (hostname : port)."
            | Log_Level _ -> "set the log level."
            | Detach _ -> "detach daemon from console."
 
// build the argument parser
let parser = UnionArgParser<Argument>()
 
// get usage text
let usage = parser.Usage()
// output:
//    --working-directory <string>: specify a working directory.
//    --listener <string> <int>: specify a listener (hostname : port).
//    --log-level <int>: set the log level.
//    --detach: detach daemon from console.
//    --help [-h|/h|/help|/?]: display this list of options.
 
// parse given input
let results = parser.Parse([| "--detach" ; "--listener" ; "localhost" ; "8080" |])
 
// get all parsed results:
// [ Detach ; Listener ("localhost", 8080) ]
let all : Argument list = results.GetResults()
```

## Querying Parameters

While getting a single list of all parsed results might be useful for some cases, 
it is more likely that you need to query the results for specific parameters:
```fsharp
let detach : bool = results.Contains <@ Detach @>
let listener : (string * int) list = results.GetResults <@ Listener @>

// returns the last observed result of this parameter
let logLevel : int = results.GetResult (<@ Log_Level @>, defaultValue = 0)
```
Querying using quotations enables a simple and type safe way 
to deconstruct parse results into their constituent values.

## Customization

The parsing behaviour of the configuration parameters 
can be customized by fixing attributes to the union cases:

```fsharp
type Arguments =
  | [<Mandatory>] Working_Directory of string
  | [<NoCommandLine>] Connection_String of string
  | [<AltCommandLine("-p")>] Port of int
```
In this case,
* `Mandatory`: parser will fail if no configuration for this parameter is given.
* `NoCommandLine`: restricts this parameter to the AppSettings section.
* `AltCommandLine`: specifies an alternative command line switch.

The following attributes are also available:

* `NoAppConfig`: restricts to command line.
* `Rest`: all remaining command line args are consumed by this parameter.
* `Hidden`: do not display in the help text.
* `GatherAllSources`: command line does not override AppSettings.
* `ParseCSV`: AppSettings entries are given as comma separated values.
* `CustomAppSettings`: sets a custom key name for AppSettings.
* `First`: Argument can only be placed at the beginning of the command line.

## Post Processing

It should be noted here that arbitrary unions are not supported by the parser. 
Union cases can only contain fields of certain primitive types, 
that is `int`, `bool`, `string` and `float`. This means of course that user-defined 
parsers are not supported. For configuration inputs that are non-trivial, 
a post-process facility is provided.
```fsharp
let parsePort p = 
    if p < 0 || p > int UInt16.MaxValue then 
        failwith "invalid port number."
    else p
 
let ports : int list = results.PostProcessResults (<@ Port @>, parsePort)
```
This construct is useful since exception handling is performed by the arg parser itself.

## Unparsing Support

UnionArgParser is convenient when it comes to automated process spawning:
```fsharp
open System.Diagnostics

let arguments : string [] = parser.PrintCommandLine [ Port 42 ; Working_Directory "temp" ]

Process.Start("foo.exe", String.concat " " arguments)
```
It can also be used to auto-generate a suitable `AppSettings` configuration file:
```fsharp
let xml : string = parser.PrintAppSettings [ Port 42 ; Working_Directory "/tmp" ]
```
which would yield the following:
```xml
<?xml version="1.0" encoding="utf-16"?>
<configuration>
  <appSettings>
    <!-- sets the port number. -->
    <add key="port" value="42" />
    <!-- sets the working directory. -->
    <add key="working directory" value="/tmp" />
  </appSettings>
</configuration>
```
