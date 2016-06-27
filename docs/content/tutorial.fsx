(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/net40"
#r "Argu.dll"

open System

(**

# Tutorial

## Introduction

The library is based on the simple observation that 
configuration parameters can be naturally described using discriminated unions. 
For instance:

*)

type Arguments =
    | Working_Directory of path:string
    | Listener of host:string * port:int
    | Log_Level of level:int
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

[ Working_Directory "/var/run" ; Listener("localhost", 8080) ; Detach ]

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

Both XML configuration and command line arguments can be parsed
at the same time. By default, command line parameters override
their corresponding XML configuration.

## Basic Usage

A minimal parser based on the above example can be created as follows:
*)

open Argu

type CLIArguments =
    | Working_Directory of path:string
    | Listener of host:string * port:int
    | Data of base64:byte[]
    | Port of tcp_port:int
    | Log_Level of level:int
    | Detach
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Listener _ -> "specify a listener (hostname : port)."
            | Data _ -> "binary data in base64 encoding."
            | Port _ -> "specify a primary port."
            | Log_Level _ -> "set the log level."
            | Detach _ -> "detach daemon from console."
 
(** We extract the argument parser from the template using the following command: *)

let parser = ArgumentParser.Create<CLIArguments>(programName = "gadget.exe")
 
(** We can get the automatically generated usage string by typing *)

let usage = parser.Usage()

(** giving

    [lang=bash]
    USAGE: gadget.exe [--working-directory <path>] [--listener <host> <port>] [--data <base64>] [--port <tcp port>] [--log-level <level>] [--detach] [--help]

    OPTIONS:

	    --working-directory <path>: specify a working directory.
	    --listener <host> <port>: specify a listener (hostname : port).
	    --data <base64>: binary data in base64 encoding.
	    --port <tcp port>: specify a primary port.
	    --log-level <level>: set the log level.
	    --detach: detach daemon from console.
	    --help: display this list of options.
 
To parse a command line input:

*)

let results = parser.Parse [| "--detach" ; "--listener" ; "localhost" ; "8080" |]
 
(** which gives *)

let all = results.GetAllResults() // [ Detach ; Listener ("localhost", 8080) ]

(**

## Querying Parameters

While getting a single list of all parsed results might be useful for some cases, 
it is more likely that you need to query the results for specific parameters:

*)

let detach = results.Contains <@ Detach @>
let listener = results.GetResults <@ Listener @>

// returns the last observed result of this parameter
let logLevel = results.GetResult (<@ Log_Level @>, defaultValue = 0)

(**

Querying using quotations enables a simple and type safe way 
to deconstruct parse results into their constituent values.

## Customization

The parsing behaviour of the configuration parameters 
can be customized by fixing attributes to the union cases:

*)

type Argument =
    | [<Mandatory>] Cache_Path of path:string
    | [<NoCommandLine>] Connection_String of conn:string
    | [<Unique>] Listener of host:string * port:int
    | [<EqualsAssignment>] Assignment of value:string
    | [<AltCommandLine("-pP")>] Primary_Port of tcp_port:int

(**

In this case,

  * `Mandatory`: parser will fail if no configuration for this parameter is given.

  * `NoCommandLine`: restricts this parameter to the AppSettings section.

  * `AltCommandLine`: specifies an alternative command line switch.

  * `EqualsAssignment` : enforces `--assignment=value` and `--assignment key=value` CLI syntax.

  * `Unique` : parser will fail if CLI provides this argument more than once.

The following attributes are also available:

  * `NoAppSettings`: restricts to command line.

  * `Rest`: all remaining command line args are consumed by this parameter.

  * `Hidden`: do not display in the help text.

  * `GatherAllSources`: command line does not override AppSettings.

  * `ParseCSV`: AppSettings entries are given as comma separated values.

  * `CustomAppSettings`: sets a custom key name for AppSettings.

  * `First`: Argument can only be placed at the beginning of the command line.

## Post Processing

It should be noted here that arbitrary unions are not supported by the parser. 
Union cases can only contain fields of primitive types. This means that user-defined 
parsers are not supported. For configuration inputs that are non-trivial, 
a post-process facility is provided.
*)

let parsePort p = 
    if p < 0 || p > int UInt16.MaxValue then 
        failwith "invalid port number."
    else p
 
let ports = results.PostProcessResults (<@ Port @>, parsePort)

(**

This construct is useful since exception handling is performed by the arg parser itself.

## Unparsing Support

Argu is convenient when it comes to automated process spawning:
*)

open System.Diagnostics

let arguments = parser.PrintCommandLineFlat [ Port 42 ; Working_Directory "temp" ]

Process.Start("foo.exe", arguments)

(**
It can also be used to auto-generate a suitable `AppSettings` configuration file:
*)

let xml = parser.PrintAppSettings [ Port 42 ; Working_Directory "/tmp" ]

(**
which would yield the following:

    [lang=xml]
    <?xml version="1.0" encoding="utf-16"?>
    <configuration>
      <appSettings>
        <!-- sets the port number. : port -->
        <add key="port" value="42" />
        <!-- sets the working directory. : path -->
        <add key="working directory" value="/tmp" />
      </appSettings>
    </configuration>
*)