module Argu.Samples.Introspect.Arguments

open Argu

type CommitArgs =
    | [<Mandatory>] Message of string
    | [<AltCommandLine("-a")>] All
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Message _ -> "commit message"
            | All -> "stage tracked files before committing"

type PushArgs =
    | [<AltCommandLine("-f")>] Force
    | Remote of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Force -> "force-push the branch"
            | Remote _ -> "remote name (default: origin)"

type GitArgs =
    | [<AltCommandLine("-v"); Inherit>] Verbose
    | [<CliPrefix(CliPrefix.None)>] Commit of ParseResults<CommitArgs>
    | [<CliPrefix(CliPrefix.None)>] Push of ParseResults<PushArgs>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Verbose -> "verbose output"
            | Commit _ -> "record a snapshot of the working tree"
            | Push _ -> "send local commits to a remote"
