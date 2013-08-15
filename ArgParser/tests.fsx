#r "bin/Debug/UnionArgParser.dll"

open UnionArgParser

//[<Mandatory>]
type CLArgs =
    | [<Mandatory>][<NoAppSettings>] Host of string
    | Set of string * int
    | Port of int
    | [<First>] Working_Directory of string
    | [<NoCommandLine>][<Mandatory>] Enable_MBrace
    | [<Rest>] Ports of int
    | Detach
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Host _ -> "sets the hostname."
            | Port _ -> "sets the port number."
            | Set _ -> "declares key/value pairs"
            | Working_Directory _ -> "sets the working directory."
            | Enable_MBrace -> "enable mbrace."
            | Ports _ -> "append ports."
            | Detach -> "make the daemon detach from console."


let ap = UnionArgParser<CLArgs>("USAGE: mbraced.exe [options]")

// get usage string
ap.Usage()

// get a command line string
ap.PrintCommandLine [ Set ("answer", 42) ; Port 2654 ; Detach ; Working_Directory "/tmp" ]

// get xml config template
ap.PrintAppSettings [ Set ("answer", 42) ; Port 2654 ; Detach ; Working_Directory "/tmp" ]

let dummy =
    [| "--working-directory" ; "C:/temp" ; "--set" ; "foo" ; "12" ; "--set" ; "bar" ; "3" ; 
       "--port" ; "12" ; "--port" ; "-13" ; "--host" ; "localhost" ; "--detach" ; "--ports" ; "12" ; "133" 
    |]

let res = ap.Parse(inputs = dummy)

res.GetAllResults()

res.Contains <@ Detach @>

let rs = res.GetAllResults ()

res.GetResult <@ Working_Directory @>

res.GetResult <@ Set @>

res.PostProcessResults (<@ Port @>, fun x -> if x < 0 then failwithf "invalid port %d" x else x)