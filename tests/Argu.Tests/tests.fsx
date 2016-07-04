#I "../../bin/net40"
#r "Argu.dll"
#r "Argu.Tests.dll"

open System
open Argu
open Argu.Tests

type MyEnum =
    | First = 1
    | Second = 2
    | Third = 3

type PushArgs =
    | All
    | Prune
    | [<AltCommandLine("-f")>]Force
    | [<AltCommandLine("-v")>]Verbose
    | Remote of repository:string
    | Branch of branch:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "Push all branches (i.e. refs under refs/heads/); cannot be used with other <refspec>."
            | Prune -> "Remove remote branches that don't have a local counterpart."
            | Verbose -> "Run verbosely."
            | Force -> "Usually, the command refuses to update a remote ref that is not an ancestor of the local ref used to overwrite it."
            | Remote _ -> "Specify a remote repository to push to."
            | Branch _ -> "Specify a branch to push changes to."

[<CliPrefix(CliPrefix.Dash)>]
type CleanArgs =
    | D
    | [<AltCommandLine("--force")>]F
    | X
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | D -> "Remove untracked directories in addition to untracked files"
            | F -> "If the Git configuration variable clean.requireForce is not set to false, git clean will refuse to delete files or directories unless given -f."
            | X -> "Remove only files ignored by Git. This may be useful to rebuild everything from scratch, but keep manually created files."

type GitArgs =
    | [<Unique>] Listener of address:string * number:int
    | Log_Level of level:int
    | [<CliPrefix(CliPrefix.None)>]Push of options:ParseResult<PushArgs>
    | [<CliPrefix(CliPrefix.None)>]Clean of suboptions:ParseResult<CleanArgs>
    | [<AltCommandLine("-E")>][<EqualsAssignment>]Environment_Variable of key:string * value:string
    | Ports of tcp_port:int list
    | Optional of num:int option
    | [<CustomAssignment("::")>] Options of MyEnum option
    | [<Inherit>] Silent
    | [<CustomCommandLine("--silent:assasin")>]Silent_Assasin
with 
    interface IArgParserTemplate with 
        member this.Usage =
            match this with
            | Listener _ -> "Specify a listener host/port combination."
            | Log_Level _ -> "Specify a log level for the process."
            | Push _ -> "Pushes changes to remote repo. See 'gadget push --help' for more info."
            | Clean _ -> "Cleans the local repo. See 'gadget clean --help' for more info."
            | Environment_Variable _ -> "Specifies an environment variable for the process."
            | Ports _ -> "Specifies a collection of port for the process."
            | Optional _ -> "just an optional parameter."
            | Options _ -> "enumeration of options."
            | Silent -> "just be silent."
            | _ -> ""

let parser = ArgumentParser.Create<GitArgs>(programName = "gadget", helpTextMessage = "Gadget -- my awesome CLI tool")

parser.PrintCommandLineArgumentsFlat [Options(Some MyEnum.First) ; Push(toParseResults [Remote "origin" ; Branch "master"])]

let result = parser.Parse [| "--silent=assasin" |]
let cresult = result.GetResult <@ Clean @>

let pparser = parser.GetSubCommandParser <@ Push @>
let cparser = parser.GetSubCommandParser <@ Clean @>

parser.PrintUsage() |> Console.WriteLine
pparser.PrintUsage() |> Console.WriteLine
cparser.PrintUsage() |> Console.WriteLine

parser.PrintCommandLineSyntax(usageStringCharacterWidth = 1000) |> Console.WriteLine
pparser.PrintCommandLineSyntax() |> Console.WriteLine
cparser.PrintCommandLineSyntax() |> Console.WriteLine

open System
open System.Collections.Generic
open System.Linq

type PrefixDictionary<'Value>(keyVals : seq<string * 'Value>) =
    /// arrays sorted by key to perform binary searches
    let keys, values = 
        keyVals
            .OrderBy((fun (key,_) -> key), StringComparer.Ordinal)
            |> Seq.toArray
            |> Array.unzip

    /// Gets the value corresponding to supplied key
    member __.Item(key : string) =
        let mutable kr = null
        let mutable vr = Unchecked.defaultof<_>
        if __.TryGetPrefix(key, &kr, &vr) && kr = key then vr
        else
            raise <| new KeyNotFoundException(key)

    member __.Get x =
        let mutable kr = null
        let mutable vr = Unchecked.defaultof<_>
        if __.TryGetPrefix(x, &kr, &vr) then Some(kr,vr) else None

    /// Look up best matching key entry by prefix
    member __.TryGetPrefix(value : string, kresult : byref<string>, vresult : byref<'Value>) : bool =
        if values.Length = 0 then false else
        let rec aux s e =
            match e - s with
            | 0 ->
                if value.StartsWith keys.[e] then e
                else -1

            | 1 ->
                if value.StartsWith keys.[e] then e
                elif value.StartsWith keys.[s] then s
                else -1

            | _ ->
                let m = (s + e) / 2
                match String.CompareOrdinal(value, keys.[m]) with
                | n when n < 0 -> aux s m
                | 0 -> m
                | _ -> aux m e

        match aux 0 (keys.Length - 1) with
        | -1 -> false
        | i -> kresult <- keys.[i] ; vresult <- values.[i] ; true


type PrefixDictionary2<'Value>(keyVals : seq<string * 'Value>) =
    /// arrays sorted by key to perform binary searches
    let keys, values = 
        keyVals
            .OrderBy((fun (key,_) -> key), StringComparer.Ordinal)
            |> Seq.toArray
            |> Array.unzip

    static let gcd (x:string) (y:string) =
        let mutable i = 0
        let n = min x.Length y.Length
        while i < n && x.[i] = y.[i] do i <- i + 1
        i

    static let tryGetPrefixIndex (keys : string []) (value : string) =
        let n = keys.Length
        let mutable i = 0
        let mutable maxValue = -1
        let mutable maxPos = -1
        while i < n do   
            let g = gcd keys.[i] value
            if g > maxValue then
                maxValue <- g
                maxPos <- i

            i <- i + 1

        maxPos

    member __.TryGetPrefix(value : string, kresult : byref<string>, vresult : byref<'Value>) : bool =
        match tryGetPrefixIndex keys value with
        | -1 -> false
        | i -> kresult <- keys.[i] ; vresult <- values.[i] ; true

    member __.Get x =
        let mutable kr = null
        let mutable vr = Unchecked.defaultof<_>
        if __.TryGetPrefix(x, &kr, &vr) then Some(kr,vr) else None




let pf1 = ["" ; "a" ; "ab" ; "abc"] |> Seq.map (fun i -> i,i) |> PrefixDictionary
let pf2 = ["" ; "a" ; "ab" ; "abc"] |> Seq.map (fun i -> i,i) |> PrefixDictionary2

#time



let gcd (x:string) (y:string) =
    let mutable i = 0
    let n = min x.Length y.Length
    while i < n && x.[i] = y.[i] do i <- i + 1
    i

#time "on"

for i = 1 to 1000000 do
    let _ = gcd "abcdef" "abcdefg"
    ()

for i = 1 to 1000000 do
    let _ = "abcdefg".StartsWith("abcdef", StringComparison.Ordinal)
    ()