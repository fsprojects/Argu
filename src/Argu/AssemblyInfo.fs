namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("2.1")>]
[<assembly: AssemblyFileVersionAttribute("2.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.1"
