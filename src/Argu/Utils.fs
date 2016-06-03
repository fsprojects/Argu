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

    /// gets the top-Level methodInfo call in a quotation
    let rec getMethod =
        function
        | Lambda(_,e) -> getMethod e
        | Call(_,f,_) -> f
        | _ -> invalidArg "expr" "quotation is not of method."


    [<RequireQualifiedAccess>]
    module internal Enum =

        let inline hasFlag (flag : ^Enum) (value : ^Enum) = flag &&& value = value

    /// reflected version of Unchecked.defaultof
    type Unchecked =
        static member DefaultOf<'T> () = Unchecked.defaultof<'T>
        static member UntypedDefaultOf(t : Type) =
            typeof<Unchecked>
                .GetMethod("DefaultOf", BindingFlags.NonPublic ||| BindingFlags.Static)
                .MakeGenericMethod([| t |])
                .Invoke(null, [||])

    type UnionCaseInfo with
        member uci.GetAttrs<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

            let attrs = uci.GetCustomAttributes(typeof<'T>) |> Seq.map (fun o -> o :?> 'T)

            if includeDeclaringTypeAttrs then
                let parentAttrs = uci.DeclaringType.GetCustomAttributes(typeof<'T>, false)  |> Seq.map (fun o -> o :?> 'T)
                Seq.append parentAttrs attrs |> Seq.toList
            else
                Seq.toList attrs

        member uci.ContainsAttr<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
            let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

            if includeDeclaringTypeAttrs then
                uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) |> Seq.isEmpty |> not
                    || uci.GetCustomAttributes(typeof<'T>) |> Seq.isEmpty |> not
            else
                uci.GetCustomAttributes(typeof<'T>) |> Seq.isEmpty |> not

    [<RequireQualifiedAccess>]
    module List =
        /// fetch last element of a non-empty list
        let rec last xs =
            match xs with
            | [] -> invalidArg "xs" "input list is empty."
            | [x] -> x
            | _ :: rest -> last rest

        /// try fetching last element of a list
        let rec tryLast xs =
            match xs with
            | [] -> None
            | [x] -> Some x
            | _ :: rest -> tryLast rest

        /// <summary>
        ///     returns `Some (map f ts)` iff `(forall t) ((f t).IsSome)`
        /// </summary>
        /// <param name="f"></param>
        /// <param name="ts"></param>
        let tryMap (f : 'T -> 'S option) (ts : 'T list) : 'S list option =
            let rec gather acc rest =
                match rest with
                | [] -> Some <| List.rev acc
                | h :: t ->
                    match f h with
                    | Some s -> gather (s :: acc) t
                    | None -> None

            gather [] ts

        /// Map active pattern combinator
        let (|Map|) f xs = List.map f xs

        /// Nondeterministic Map active pattern combinator
        let (|TryMap|_|) f xs = tryMap f xs

    [<RequireQualifiedAccess>]
    module Boolean =
        let tryParse (inp : string) =
            let ok, b = Boolean.TryParse inp
            if ok then Some b
            else None
            
    type IDictionary<'K,'V> with
        member d.TryFind(k : 'K) =
            let mutable v = Unchecked.defaultof<'V>
            if d.TryGetValue(k, &v) then Some v else None


    /// inherit this type for easy comparison semantics
    type ProjectionComparison<'Id, 'Cmp when 'Cmp : comparison> (token : 'Cmp) =
        member private __.ComparisonToken = token
        interface IComparable with
            member x.CompareTo y =
                match y with
                | :? ProjectionComparison<'Id, 'Cmp> as y -> compare token y.ComparisonToken
                | _ -> invalidArg "y" "invalid comparand."

        override x.Equals y =
            match y with
            | :? ProjectionComparison<'Id, 'Cmp> as y -> token = y.ComparisonToken
            | _ -> false

        override x.GetHashCode() = hash token

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