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

let usage = parser.PrintUsage()

(** giving

    [lang=bash]
    USAGE: gadget.exe [--help] [--working-directory <path>] [--listener <host> <port>] [--data <base64>]
                      [--port <tcp port>] [--log-level <level>] [--detach]

    OPTIONS:

        --working-directory <path>
                              specify a working directory.
        --listener <host> <port>
                              specify a listener (hostname : port).
        --data <base64>       binary data in base64 encoding.
        --port <tcp port>     specify a primary port.
        --log-level <level>   set the log level.
        --detach              detach daemon from console.
        --help                display this list of options.
 
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
let dataOpt = results.TryGetResult <@ Data @>
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
    | [<AltCommandLine("-p")>] Primary_Port of tcp_port:int

(**

In this case,

  * `Mandatory`: parser will fail if no configuration for this parameter is given.

  * `NoCommandLine`: restricts this parameter to the AppSettings section.

  * `AltCommandLine`: specifies an alternative command line switch.

  * `EqualsAssignment` : enforces `--assignment=value` and `--assignment key=value` CLI syntax.

  * `Unique` : parser will fail if CLI provides this argument more than once.

The following attributes are also available:

  * `NoAppSettings`: restricts argument to CLI parsing only.

  * `Hidden`: do not display in the help usage string.

  * `GatherAllSources`: CLI arguments will not override AppSettings parameters.

  * `CustomAppSettings`: sets a custom key name for AppSettings.

  * `First`: Argument can only be placed at the beginning of the command line.

For a complete list of all attributes provided by Argu, 
please see http://fsprojects.github.io/Argu/reference/argu-arguattributes.html.

## Optional and List parameters

It is possible to specify argument parameters that are either optional or lists:

*)

type VariadicParameters =
    | [<EqualsAssignment>] Enable_Logging of path:string option
    | Tcp_Ports of port:int list

(**

which results in the following syntax:

    [lang=console]
    USAGE: gadget.exe [--help] [--enable-logging[=<path>]] [--tcp-ports [<port>...]]

    OPTIONS:

        --enable-logging[=<path>]  enable logging for the process; optionally path to the logfile can be specified.
        --tcp-ports [<port>...]    specify a list of TCP ports for the process.
        --help                     display this list of options.

## SubCommand parsing

As of Argu 3.0, it is possible to provide nested, contextual parsing.
For example, consider this mock git CLI syntax:

*)

[<CliPrefix(CliPrefix.Dash)>]
type CleanArgs =
    | D
    | F
    | X
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | D -> "Remove untracked directories in addition to untracked files"
            | F -> "Git clean will refuse to delete files or directories unless given -f."
            | X -> "Remove only files ignored by Git."

and CommitArgs =
    | Amend
    | [<AltCommandLine("-p")>]Patch
    | [<AltCommandLine("-m")>]Message of msg:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Amend -> "Replace the tip of the current branch by creating a new commit."
            | Patch -> "Use the interactive patch selection interface to chose which changes to commit."
            | Message _ -> "Use the given <msg> as the commit message. "

and GitArgs =
    | Version
    | [<AltCommandLine("-v")>]Verbose
    | [<CliPrefix(CliPrefix.None)>] Clean of ParseResult<CleanArgs>
    | [<CliPrefix(CliPrefix.None)>] Commit of ParseResult<CommitArgs>
with
    interface IArgParserTemplate with
        member this.Usage = 
            match this with
            | Version -> "Prints the Git suite version that the git program came from."
            | Verbose -> "Print a lot of output to stdout."
            | Clean _ -> "Remove untracked files from the working tree."
            | Commit _ -> "Record changes to the repository."


let parser = ArgumentParser.Create<GitArgs>(programName = "git")

parser.PrintUsage() |> Console.WriteLine

parser.Parse [|"-f"|]

(**

which generates the following syntax:

    [lang=console]
    USAGE: git [--help] [--version] [--verbose] [<subcommand> [<options>]]

    SUBCOMMANDS:

        clean <options>       Remove untracked files from the working tree.
        commit <options>      Record changes to the repository.

	    Use 'git <subcommand> --help' for additional information.

    OPTIONS:

        --version             Prints the Git suite version that the git program came from.
        --verbose, -v         Print a lot of output to stdout.
        --help                display this list of options.


This allows specifying parameters that are particular to a subcommand context.
For instance, `git clean -fdx` parses correctly to `[Clean [F; D; X]]`, however
`git -f` or `git commit -f` will both result in a parse error:

    [lang=console]
    ERROR: unrecognized argument: '-f'.

### Inheriting parent arguments

Switches specified in the parent argument union do not automatically
make it to the syntax of the child subcommand. For example the command

    [lang=console]
    git clean --version

will result in parse error since `Version` is not a part of the subcommand syntax,
but one of its parent syntax. It is possible to parent options visible inside subcommands 
by attaching the [`InheritAttribute`](http://fsprojects.github.io/Argu/reference/argu-arguattributes-inheritattribute.html) 
to switches.

    [lang=fsharp]
    type GitArgs =
        | [<Inherit>] Version

which would make the aforementioned syntax valid.

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

let arguments = parser.PrintCommandLineArgumentsFlat [ Port 42 ; Working_Directory "temp" ]

Process.Start("foo.exe", arguments)

(**
It can also be used to auto-generate a suitable `AppSettings` configuration file:
*)

let xml = parser.PrintAppSettingsArguments [ Port 42 ; Working_Directory "/tmp" ]

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