[<AutoOpen>]
module internal Argu.Parser

open System
open System.Text
open System.Collections.Generic

open Microsoft.FSharp.Reflection

type CliParseState =
    abstract AllArguments : string[]
    abstract Current : string
    abstract Position : int
    abstract Next : unit -> string

[<AbstractClass>]
type Parser() =
    abstract Type : Type
    abstract ParseCliUntyped : CliParseState -> obj
    abstract UnParseCliUntyped : StringBuilder -> obj -> unit

    abstract ParseUntyped : string -> obj
    abstract UnParseUntyped : obj -> string


[<AbstractClass>]
type Parser<'T>() =
    inherit Parser()
    abstract ParseCli : CliParseState -> 'T
    abstract UnParseCli : StringBuilder -> 'T -> unit
    abstract Parse : string -> 'T
    abstract UnParse : 'T -> string

    override __.Type = typeof<'T>
    override __.ParseCliUntyped cps = __.ParseCli cps :> obj
    override __.UnParseCliUntyped sb o = __.UnParseCli sb (o :?> 'T)

    override __.ParseUntyped str = __.Parse str :> obj
    override __.UnParseUntyped o = __.UnParse (o :?> 'T)

[<AbstractClass>]
type PrimitiveParser<'T>() =
    inherit Parser<'T>()

    override __.ParseCli (cps : CliParseState) =
        cps.Next() |> __.Parse

    override __.UnParseCli (sb : StringBuilder) (t : 'T) = 
        sb.Append(__.UnParse t) |> ignore