(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/net40"
#r "UnionArgParser.dll"

open System

(**

# UnionArgParser

A declarative CLI argument/XML configuration parser for F# applications.
Allows quick definition of argument parsing schema through F# union declarations.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      UnionArgParser can be <a href="https://nuget.org/packages/UnionArgParser">installed from NuGet</a>:
      <pre>PM> Install-Package UnionArgParser</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

## Introduction

The library is based on the simple observation that 
configuration parameters can be naturally described using discriminated unions. 
For instance:

*)

type Arguments =
    | Working_Directory of string
    | Listener of host:string * port:int
    | Log_Level of int
    | Detach

(**

UnionArgParser takes such discriminated unions and generates 
a corresponding argument parsing scheme. 
For example, a parser generated from the above template would
take the following command line input

    [lang=bash]
    --working-directory /var/run --listener localhost 8080 --detach

and parse it into the list
*)

[ Working_Directory "/var/run" ; Listener("localhost", 8080) ; Detach ]

(**

UnionArgParser is also capable of reading the `AppSettings` section
of an application's configuration file:

    [lang=xml]
    <appSettings>
        <add key="working directory" value="C:\temp" />
        <add key="listener" value="192.168.0.3, 2675" />
        <add key="log level" value="3" />
        <add key="detach" value="true" />
    </appSettings>

## Who uses UnionArgParser?

* [MBrace framework](http://mbrace.net/)

* [Paket dependency manager](http://fsprojects.github.io/Paket/)

## Documentation

  * [Tutorial](tutorial.html) A short walkthrough of UnionArgParser features.

  * [API Reference](reference/index.html) contains automatically generated documentation for all types, 
    modules and functions in the library.

## Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests.

The library is available under the MIT License. 
For more information see the [License file][license] in the GitHub repository. 

  [gh]: https://github.com/nessos/Vagrant
  [issues]: https://github.com/nessos/Vagrant/issues
  [license]: https://github.com/nessos/Vagrant/blob/master/License.md

*)