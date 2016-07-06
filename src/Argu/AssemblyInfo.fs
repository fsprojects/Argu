namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("3.0.1")>]
[<assembly: AssemblyFileVersionAttribute("3.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "3.0.1"
    let [<Literal>] InformationalVersion = "3.0.1"
