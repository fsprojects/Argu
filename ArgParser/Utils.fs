namespace ArgParser

    open System
    open System.Collections.Generic
    open System.Text
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations.Patterns

    module internal Utils =

        let rec getMethod =
            function
            | Lambda(_,e) -> getMethod e
            | Call(_,f,_) -> f
            | _ -> invalidArg "expr" "quotation is not of method."

        /// reflected version of Unchecked.defaultof
        let defaultOf =
            let gf = (getMethod <@ Unchecked.defaultof<int> @>).GetGenericMethodDefinition()
            fun (t : Type) -> (gf.MakeGenericMethod [|t|]).Invoke(null, [||])


        type UnionCaseAttributeReader(uci : UnionCaseInfo) =
            member __.GetAttrs<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
                let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

                let attrs = uci.GetCustomAttributes(typeof<'T>) |> Seq.map (fun o -> o :?> 'T)

                if includeDeclaringTypeAttrs then
                    let parentAttrs = uci.DeclaringType.GetCustomAttributes<'T> ()
                    Seq.append parentAttrs attrs |> Seq.toList
                else
                    Seq.toList attrs

            member __.ContainsAttr<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs) =
                let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

                if includeDeclaringTypeAttrs then
                    uci.DeclaringType.GetCustomAttributes<'T> () |> Seq.isEmpty |> not
                        || uci.GetCustomAttributes(typeof<'T>) |> Seq.isEmpty |> not
                else
                    uci.GetCustomAttributes(typeof<'T>) |> Seq.isEmpty |> not

        [<RequireQualifiedAccess>]
        module List =
            let rec last xs =
                match xs with
                | [] -> invalidArg "xs" "input list is empty."
                | [x] -> x
                | _ :: rest -> last rest

            let rec tryLast xs =
                match xs with
                | [] -> None
                | [x] -> Some x
                | _ :: rest -> tryLast rest

            let tryMap (f : 'T -> 'S option) (ts : 'T list) : 'S list option =
                let rec gather acc rest =
                    match rest with
                    | [] -> Some <| List.rev acc
                    | h :: t ->
                        match f h with
                        | Some s -> gather (s :: acc) t
                        | None -> None

                gather [] ts

            let tryFind2 (f : 'T -> 'S -> bool) (xs : 'T list) (ys : 'S list) =
                let rec aux xs =
                    match xs with
                    | [] -> None
                    | x :: xs' ->
                        match List.tryFind (f x) ys with
                        | None -> aux xs'
                        | Some y -> Some(x,y)

                aux xs

            let (|Map|) f xs = List.map f xs
            let (|TryMap|_|) f xs = tryMap f xs

        [<RequireQualifiedAccess>]
        module Boolean =
            let tryParse (inp : string) =
                let r = ref false
                if Boolean.TryParse(inp, r) then Some r.Value
                else None
            
        type IDictionary<'K,'V> with
            member d.TryFind(k : 'K) =
                let vr = ref Unchecked.defaultof<'V>
                if d.TryGetValue(k, vr) then Some vr.Value else None


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

        // string monad

        type StringBuilderM = StringBuilder -> unit

        type StringExprBuilder () =
            member __.Zero () : StringBuilderM = ignore
            member __.Yield (txt : string) : StringBuilderM = fun b -> b.Append txt |> ignore
            member __.Yield (c : char) : StringBuilderM = fun b -> b.Append c |> ignore
            member __.YieldFrom f = f : StringBuilderM

            member __.Combine(f : StringBuilderM, g : StringBuilderM) = fun b -> f b; g b
            member __.Delay (f : unit -> StringBuilderM) = fun b -> f () b
        
            member __.For (xs : 'a seq, f : 'a -> StringBuilderM) =
                fun b ->
                    let e = xs.GetEnumerator ()
                    while e.MoveNext() do f e.Current b

            member __.While (p : unit -> bool, f : StringBuilderM) =
                fun b -> while p () do f b

        let string = new StringExprBuilder ()

        [<RequireQualifiedAccess>]
        module String =
            let build (f : StringBuilderM) =
                let b = new StringBuilder ()
                do f b
                b.ToString ()