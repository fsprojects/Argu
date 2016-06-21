namespace Argu

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Reflection

open System.Xml
open System.Xml.Linq

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns

[<AutoOpen>]
module internal Utils =

    let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance

    [<RequireQualifiedAccess>]
    module Enum =

        let inline hasFlag (flag : ^Enum) (value : ^Enum) = flag &&& value = value

    [<AbstractClass>]
    type Existential internal () =
        static let genTy = typedefof<Existential<_>>
        abstract Type : Type
        abstract Accept : IFunc<'R> -> 'R
        static member FromType(t : Type) =
            let et = genTy.MakeGenericType [|t|]
            Activator.CreateInstance et :?> Existential

    and Existential<'T> () =
        inherit Existential()
        override __.Type = typeof<'T>
        override __.Accept func = func.Invoke<'T>()

    and IFunc<'R> =
        abstract Invoke<'T> : unit -> 'R

    [<AbstractClass>]
    type ShapeArgumentTemplate() =
        static let genTy = typedefof<ShapeArgumentTemplate<_>>
        abstract Type : Type
        abstract Accept : ITemplateFunc<'R> -> 'R
        static member FromType(t : Type) =
            let et = genTy.MakeGenericType [|t|]
            Activator.CreateInstance et :?> ShapeArgumentTemplate

    and ShapeArgumentTemplate<'Template when 'Template :> IArgParserTemplate>() =
        inherit ShapeArgumentTemplate()
        override __.Type = typeof<'Template>
        override __.Accept func = func.Invoke<'Template>()

    and ITemplateFunc<'R> =
        abstract Invoke<'Template when 'Template :> IArgParserTemplate> : unit -> 'R

    /// reflected version of Unchecked.defaultof
    type Unchecked =
        static member UntypedDefaultOf(t : Type) =
            Existential.FromType(t).Accept { 
                new IFunc<obj> with 
                    member __.Invoke<'T> () = Unchecked.defaultof<'T> :> obj
                }

    type MemberInfo with
        member m.ContainsAttribute<'T when 'T :> Attribute> () =
            m.GetCustomAttributes(typeof<'T>,true) |> Array.isEmpty |> not

    type IDictionary<'K,'V> with
        member d.TryFind k =
            let ok,found = d.TryGetValue k
            if ok then Some found
            else None

    type UnionCaseInfo with
        member uci.GetAttributes<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs : bool) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

            let caseAttrs = uci.GetCustomAttributes typeof<'T>
            let attrs =
                if includeDeclaringTypeAttrs then
                    uci.DeclaringType.GetCustomAttributes(typeof<'T>, false)
                    |> Seq.append caseAttrs
                else
                    caseAttrs :> _

            attrs |> Seq.map (fun o -> o :?> 'T)

        member uci.TryGetAttribute<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs : bool) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

            match uci.GetCustomAttributes typeof<'T> with
            | [||] when includeDeclaringTypeAttrs ->
                match uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) with
                | [||] -> None
                | attrs -> Some (attrs.[0] :?> 'T)
            | [||] -> None
            | attrs -> Some (attrs.[0] :?> 'T)

        member uci.ContainsAttribute<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs : bool) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false
            if uci.GetCustomAttributes typeof<'T> |> Array.isEmpty |> not then true
            elif includeDeclaringTypeAttrs then
                uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) |> Array.isEmpty |> not
            else
                false

    /// recognize exprs that strictly contain DU constructors
    /// e.g. <@ Case @> is valid but <@ fun x y -> Case y x @> is invalid
    let expr2Uci (e : Expr) =
        let (|Vars|_|) (exprs : Expr list) =
            let vars = exprs |> List.choose (|Var|_|)
            if vars.Length = exprs.Length then Some vars
            else None

        let rec aux (tupledArg : Var option) vars (e : Expr) =
            match tupledArg, e with
            | None, Lambda(arg, b) -> aux (Some arg) vars b
            | Some arg, Let(x, TupleGet(Var varg, _), b) when arg = varg -> aux tupledArg (x :: vars) b
            | None, NewUnionCase(u, []) -> u
            | Some a, NewUnionCase(u, [Var x]) when a = x -> u
            | Some _, NewUnionCase(u, Vars args) when vars.Length > 0 && List.rev vars = args -> u
            | _ -> invalidArg "expr" "Only union constructors are permitted in expression based queries."

        aux None [] e

    [<RequireQualifiedAccess>]
    module Array =

        let last (ts : 'T[]) =
            match ts.Length with
            | 0 -> invalidArg "xs" "input array is empty."
            | n -> ts.[n - 1]

    [<RequireQualifiedAccess>]
    module List =

        /// try fetching last element of a list
        let rec tryLast xs =
            match xs with
            | [] -> None
            | [x] -> Some x
            | _ :: rest -> tryLast rest

    [<RequireQualifiedAccess>]
    module Seq =

        /// try fetching last element of a sequence
        let tryLast (xs : seq<'T>) =
            let mutable isAccessed = false
            let mutable current = Unchecked.defaultof<'T>
            for x in xs do isAccessed <- true; current <- x
            if isAccessed then Some current else None

    // string builder compexpr

    type StringExpr = StringBuilder -> unit

    type StringExprBuilder () =
        member __.Zero () : StringExpr = ignore
        member __.Yield (txt : string) : StringExpr = fun b -> b.Append txt |> ignore
        member __.Yield (c : char) : StringExpr = fun b -> b.Append c |> ignore
        member __.YieldFrom f = f : StringExpr

        member __.Combine(f : StringExpr, g : StringExpr) : StringExpr = fun b -> f b; g b
        member __.Delay (f : unit -> StringExpr) : StringExpr = fun b -> f () b
        
        member __.For (xs : 'a seq, f : 'a -> StringExpr) : StringExpr =
            fun b ->
                use e = xs.GetEnumerator ()
                while e.MoveNext() do f e.Current b

        member __.While (p : unit -> bool, f : StringExpr) : StringExpr =
            fun b -> while p () do f b

    let stringExpr = new StringExprBuilder ()

    [<RequireQualifiedAccess>]
    module String =
        let build (f : StringExpr) =
            let b = new StringBuilder ()
            do f b
            b.ToString ()