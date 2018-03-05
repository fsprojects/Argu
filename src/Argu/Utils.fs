[<AutoOpen>]
module internal Argu.Utils

open System
open System.Collections.Generic
open System.Reflection
open System.Text
open System.Text.RegularExpressions

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open System.Collections

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

    /// try fetching first element of a sequence
    let tryFirst (xs : seq<'T>) =
        let en = xs.GetEnumerator()
        if en.MoveNext() then Some en.Current
        else None

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

[<RequireQualifiedAccess>]
module String =
    let inline mkWhiteSpace (length : int) = new String(' ', length)

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
    member m.ContainsAttribute<'T when 'T :> Attribute> () : bool=
        m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

    member m.TryGetAttribute<'T when 'T :> Attribute> () : 'T option =
        match m.GetCustomAttributes(typeof<'T>, true) |> Seq.toArray with
        | [||] -> None
        | attrs -> attrs |> Array.last :?> 'T |> Some

type IDictionary<'K,'V> with
    member d.TryFind k =
        let ok,found = d.TryGetValue k
        if ok then Some found
        else None

let currentProgramName = lazy(System.Diagnostics.Process.GetCurrentProcess().MainModule.ModuleName)

type UnionCaseInfo with
    member uci.GetAttributes<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs : bool) =
        let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

        let caseAttrs = uci.GetCustomAttributes typeof<'T> |> Seq.cast<Attribute>
        let attrs =
            if includeDeclaringTypeAttrs then
                uci.DeclaringType.GetCustomAttributes(typeof<'T>, false)
                |> Seq.cast<Attribute>
                |> Seq.append caseAttrs
            else
                caseAttrs

        attrs |> Seq.map (fun o -> o :?> 'T)

    member uci.TryGetAttribute<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs : bool) =
        let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false

        match uci.GetCustomAttributes typeof<'T> with
        | [||] when includeDeclaringTypeAttrs ->
            match uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) |> Seq.toArray with
            | [||] -> None
            | attrs -> Some (attrs.[0] :?> 'T)
        | [||] -> None
        | attrs -> Some (attrs.[0] :?> 'T)

    member uci.ContainsAttribute<'T when 'T :> Attribute> (?includeDeclaringTypeAttrs : bool) =
        let includeDeclaringTypeAttrs = defaultArg includeDeclaringTypeAttrs false
        if uci.GetCustomAttributes typeof<'T> |> Array.isEmpty |> not then true
        elif includeDeclaringTypeAttrs then
            uci.DeclaringType.GetCustomAttributes(typeof<'T>, false) |> Seq.isEmpty |> not
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

//let private whitespaceRegex = new Regex(@"\s", RegexOptions.Compiled)

/// these chars can't be roundtripped through a cmd-line -> should fail with an exception
let private invalidChars = [| char 0 |]
/// these chars require quoting or escaping
let private peskyChars = [| '"' ; '\t' ; ' ' ; '\\' |]

let private doesStringContain chars (s:string) =
    s |> Seq.exists (fun c -> chars |> Seq.exists (fun cc -> cc = c))

let escapeCliString (value : string) =
//    if Environment.OSVersion.Platform = PlatformID.Win32NT then
        if value = null then
            raise (ArgumentNullException("value"))
        if value = "" then
            "\"\""
        else if value |> doesStringContain invalidChars then
            failwithf "The string can not be roundtripped."
        else if not (value |> doesStringContain peskyChars) then
            value
        else
            let valueSeq = seq {
                // always quote if we have some pesky char.
                // It may not be strictly necessary, but doesn't hurt either
                yield '"'

                for i, c in value |> Seq.mapi (fun i c -> i, c) do
                    match c with

                    | '"' ->
                        yield '\\'
                        yield '"'

                    | '\\' ->
                        (* The rules for " and \ are stupid. Source: https://github.com/ArildF/masters/blob/1542218180f2f462c604173ce8925f419155f19c/trunk/sscli/clr/src/vm/util.cpp#L1013
                            * 2N backslashes + " ==> N backslashes and begin/end quote
                            * 2N+1 backslashes + " ==> N backslashes + literal "
                            * N backslashes ==> N backslashes
                        *)
                        let nextCharAfterBackslashes = value |> Seq.skip (i + 1) |> Seq.filter (fun c -> c <> '\\') |> Seq.tryFirst
                        if nextCharAfterBackslashes = Some ('"') || nextCharAfterBackslashes = None then
                            yield '\\'
                            yield '\\'
                        else
                            yield '\\'

                    | ' ' // we quote anyway, so ' ' does not need to be treated specially.
                    | '\t' // ... same
                    | _ -> yield c

                yield '"'
            }

            valueSeq |> Seq.toArray |> String
//    else
//        if whitespaceRegex.IsMatch value then sprintf "'%s'" value
//        else value

let flattenCliTokens (tokens : seq<string>) =
    tokens |> Seq.map escapeCliString |> String.concat " "

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

    let whiteSpace len : StringExpr<unit> = fun sb -> ignore(sb.Append(String.mkWhiteSpace len))

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

    member __.Count with get(): int = keys.Length
    interface IEnumerable<string * 'Value> with
        member __.GetEnumerator() = keyVals.GetEnumerator()
        member __.GetEnumerator() = keyVals.GetEnumerator() :> IEnumerator

/// Gets the default width of the current console window,
/// if available.
let getDefaultCharacterWidth() =
    max (try Console.WindowWidth - 1 with _ -> 80) 80

// Wordwrap implementation kindly provided by @forki
let wordwrap (width:int) (inputText:string) =
    let breakLine (text:string) (pos:int) (max:int) =
        // Find last whitespace in line
        let mutable i = max - 1
        while i >= 0 && not (Char.IsWhiteSpace text.[pos + i]) do
            i <- i - 1

        if i < 0 then
            max // No whitespace found; break at maximum length
        else
            // Find start of whitespace
            while i >= 0 && Char.IsWhiteSpace text.[pos + i] do
                i <- i - 1
            // Return length of text before whitespace
            i + 1

    if width < 1 then invalidArg "width" "Must be positive number."

    let inputText = inputText.Replace("\r\n","\n").Replace("\r","\n")
    let lines = new ResizeArray<string>()
    // Parse each line of text
    let mutable pos = 0
    let mutable next = 0

    while pos < inputText.Length do
        // Find end of line
        let mutable eol = inputText.IndexOf('\n', pos)

        if eol = -1 then
            next <- inputText.Length
            eol <- inputText.Length
        else
            next <- eol + 1

        // Copy this line of text, breaking into smaller lines as needed
        if eol > pos then
            while eol > pos do
                let mutable len = eol - pos

                if len > width then
                    len <- breakLine inputText pos width

                inputText.[pos .. pos + len - 1] |> lines.Add

                // Trim whitespace following break
                pos <- pos + len

                while pos < eol && Char.IsWhiteSpace inputText.[pos] do
                    pos <- pos + 1

        pos <- next

    lines.ToArray() |> Array.toList