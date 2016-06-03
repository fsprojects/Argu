namespace Argu

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Reflection

open System.Xml
open System.Xml.Linq

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations.Patterns

[<AutoOpen>]
module internal Utils =

    let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance

    [<RequireQualifiedAccess>]
    module internal Enum =

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

    /// reflected version of Unchecked.defaultof
    type Unchecked =
        static member UntypedDefaultOf(t : Type) =
            Existential.FromType(t).Accept { 
                new IFunc<obj> with 
                    member __.Invoke<'T> () = Unchecked.defaultof<'T> :> obj
                }

    type UnionCaseInfo with
        member uci.GetAttributes<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

            let caseAttrs = uci.GetCustomAttributes typeof<'T>
            let attrs =
                if includeDeclaringTypeAttrs then
                    uci.DeclaringType.GetCustomAttributes(typeof<'T>, false)
                    |> Seq.append caseAttrs
                else
                    caseAttrs :> _

            attrs |> Seq.map (fun o -> o :?> 'T)

        member uci.TryGetAttribute<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

            match uci.GetCustomAttributes typeof<'T> with
            | [||] when includeDeclaringTypeAttrs ->
                match uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) with
                | [||] -> None
                | attrs -> Some (attrs.[0] :?> 'T)
            | [||] -> None
            | attrs -> Some (attrs.[0] :?> 'T)

        member uci.ContainsAttribute<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false
            if uci.GetCustomAttributes typeof<'T> |> Array.isEmpty |> not then true
            elif includeDeclaringTypeAttrs then
                uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) |> Array.isEmpty |> not
            else
                false

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

    /// inherit this type for easy comparison semantics
    [<AbstractClass>]
    type ProjectionComparison<'Id, 'Cmp when 'Cmp : comparison> () =
        abstract ComparisonToken : 'Cmp
        interface IComparable with
            member x.CompareTo y =
                match y with
                | :? ProjectionComparison<'Id, 'Cmp> as y -> compare x.ComparisonToken y.ComparisonToken
                | _ -> invalidArg "y" "invalid comparand."

        override x.Equals y =
            match y with
            | :? ProjectionComparison<'Id, 'Cmp> as y -> x.ComparisonToken = y.ComparisonToken
            | _ -> false

        override x.GetHashCode() = hash x.ComparisonToken

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