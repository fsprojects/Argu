module Argu.Samples.LS.Main

open System
open Argu

[<EntryPoint>]
let main argv = 
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<LsArguments>(programName = "ls", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    printfn "Got parse results %A" <| results.GetAllResults()
    let files = results.GetResult(<@ Files @>, defaultValue = [])
    printfn "Listing files %A" files

    0