#r "bin/Release/ArgParser.dll"

open ArgParser

//[<Mandatory>]
type CLArgs =
    | [<Mandatory>]Host of string
    | Set of string * int
    | Port of int
    | Working_Directory of string
    | [<NoCommandLine>][<Mandatory>] Enable_MBrace
    | [<Rest>]Ports of int
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


let ap = ArgParser<CLArgs>("USAGE: mbraced.exe [options]")

ap.Usage()

let dummy =
    [| "--set" ; "foo" ; "12" ; "--set" ; "bar" ; "3" ; "--port" ; "12" ; "--port" ; "-13" ; 
        "--working-directory" ; "C:/temp" ; "--host" ; "localhost" ; "--detach" ; "--ports" ; "12" ; "133" |]

let res = ap.Parse(inputs = dummy)

res.Contains <@ Detach @>

let rs = res.GetResults ()

ap.Print rs

res.GetResult <@ Working_Directory @>

res.GetResult <@ Set @>

res.PostProcessResults <@ Port @> (fun x -> if x < 0 then failwithf "invalid port %d" x else x)