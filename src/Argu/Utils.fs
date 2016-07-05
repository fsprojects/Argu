[<AutoOpen>]
module internal Argu.Utils

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns

let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance

let inline arguExn fmt = Printf.ksprintf(fun msg -> raise <| new ArguException(msg)) fmt

/// get CL arguments from environment
let getEnvironmentCommandLineArgs () =
    match System.Environment.GetCommandLineArgs() with
    | [||] -> [||]
    | args -> args.[1..]

[<RequireQualifiedAccess>]
module Enum =

    let inline hasFlag (flag : ^Enum) (value : ^Enum) = flag &&& value = value

[<RequireQualifiedAccess>]
module Array =

    let last (ts : 'T[]) =
        match ts.Length with
        | 0 -> invalidArg "xs" "input array is empty."
        | n -> ts.[n - 1]

    let tryLast (ts : 'T[]) =
        match ts.Length with
        | 0 -> None
        | n -> Some ts.[n-1]

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

    /// partition sequence according to predicate
    let partition (predicate : 'T -> bool) (ts : seq<'T>) =
        let l,r = new ResizeArray<'T>(), new ResizeArray<'T>()
        for t in ts do
            if predicate t then l.Add t else r.Add t

        l.ToArray(), r.ToArray()

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
        m.GetCustomAttributes(typeof<'T>, true) |> Array.isEmpty |> not

    member m.TryGetAttribute<'T when 'T :> Attribute> () =
        match m.GetCustomAttributes(typeof<'T>, true) with
        | [||] -> None
        | attrs -> attrs |> Array.last |> unbox<'T> |> Some


type IDictionary<'K,'V> with
    member d.TryFind k =
        let ok,found = d.TryGetValue k
        if ok then Some found
        else None


let currentProgramName = lazy(System.Diagnostics.Process.GetCurrentProcess().MainModule.ModuleName)

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

let private whitespaceRegex = new Regex(@"\s", RegexOptions.Compiled)
let escapeCliString (value : string) =
    if whitespaceRegex.IsMatch value then sprintf "'%s'" value
    else value

let flattenCliTokens (tokens : seq<string>) =
    tokens |> Seq.map escapeCliString |> String.concat " "

let private whitespaceAllRegex = new Regex(@"^\s*$", RegexOptions.Compiled)
/// Replacement of String.IsNullOrWhiteSpace for NET35
let isNullOrWhiteSpace (string:string) =
#if NET35
    if string = null then true
    else whitespaceAllRegex.IsMatch string
#else
    String.IsNullOrWhiteSpace string
#endif

type StringExpr<'T> = StringBuilder -> 'T

type StringExprBuilder () =
    member __.Zero () : StringExpr<unit> = ignore
    member __.Bind(f : StringExpr<'T>, g : 'T -> StringExpr<'S>) : StringExpr<'S> =
        fun sb -> g (f sb) sb

    member __.Yield (txt : string) : StringExpr<unit> = fun b -> b.Append txt |> ignore
    member __.Yield (c : char) : StringExpr<unit> = fun b -> b.Append c |> ignore
    member __.YieldFrom (f : StringExpr<unit>) = f

    member __.Combine(f : StringExpr<unit>, g : StringExpr<'T>) : StringExpr<'T> = fun b -> f b; g b
    member __.Delay (f : unit -> StringExpr<'T>) : StringExpr<'T> = fun b -> f () b
        
    member __.For (xs : 'a seq, f : 'a -> StringExpr<unit>) : StringExpr<unit> =
        fun b ->
            use e = xs.GetEnumerator ()
            while e.MoveNext() do f e.Current b

    member __.While (p : unit -> bool, f : StringExpr<unit>) : StringExpr<unit> =
        fun b -> while p () do f b

let stringExpr = new StringExprBuilder ()

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringExpr =
    let build (f : StringExpr<unit>) =
        let b = new StringBuilder ()
        do f b
        b.ToString ()

    let currentLength : StringExpr<int> = fun sb -> sb.Length

    let whiteSpace len : StringExpr<unit> = fun sb -> ignore(sb.Append(String(' ', len)))

/// Dictionary enabling lookups by string prefix
/// e.g. the string '--foo=bar' can be used to look up the key '--foo'
type PrefixDictionary<'Value>(keyVals : seq<string * 'Value>) =
    // could probably use a trie here, but cli arg names are relatively few
    // with relatively flat structure to get any real perf benefits
    let keys, values = 
        keyVals
        |> Seq.toArray
        |> Array.unzip

    /// Gets the value corresponding to supplied key
    member __.Item(key : string) =
        let mutable kr = null
        let mutable vr = Unchecked.defaultof<_>
        if __.TryGetPrefix(key, &kr, &vr) && kr = key then vr
        else
            raise <| new KeyNotFoundException(key)

    /// Look up best matching key entry by prefix
    member __.TryGetPrefix(value : string, kresult : byref<string>, vresult : byref<'Value>) : bool =
        // Just iterate through all the keys, picking the matching prefix
        // with the maximal length
        let mutable maxPos = -1
        let mutable maxLen = -1
        for i = 0 to keys.Length - 1 do
            let key = keys.[i]
            let pLength =
                if value.StartsWith(key, StringComparison.Ordinal) then key.Length
                else -1

            if pLength > maxLen then
                maxPos <- i
                maxLen <- pLength

        if maxPos < 0 then false
        else kresult <- keys.[maxPos] ; vresult <- values.[maxPos] ; true