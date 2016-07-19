// Learn more about F# at http://fsharp.org

open System
open Argu

type CLIArguments =
    | Working_Directory of string
    | Listener of host:string * port:int
    | Data of byte []
    | Port of int
    | Log_Level of int
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
 
[<EntryPoint>]
let main argv = 
    printfn "Hello World!"
    printfn "%A" argv

    // build the argument parser
    let parser = ArgumentParser.Create<CLIArguments>()
     
    // get usage text
    let usage = parser.PrintUsage()
    // output:
    //    --working-directory <string>: specify a working directory.
    //    --listener <host:string> <port:int>: specify a listener (hostname : port).
    //    --log-level <int>: set the log level.
    //    --detach: detach daemon from console.
    //    --help [-h|/h|/help|/?]: display this list of options.

    printfn "Usage:"
    printfn "%s" usage
    printfn ""
     
    // parse given input
    let results = parser.Parse([| "--detach" ; "--listener" ; "localhost" ; "8080" |])

    printfn "Results:"
    printfn "%A" results
    printfn ""
     
    // get all parsed results
    let all = results.GetAllResults() // [ Detach ; Listener ("localhost", 8080) ]

    printfn "All results:"
    printfn "%A" all
    printfn ""

    0 // return an integer exit code
