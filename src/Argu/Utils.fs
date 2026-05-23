[<AutoOpen>]
module internal Argu.Utils

open System
open System.Collections.Generic
open System.Reflection
open System.Text

open FSharp.Quotations
open FSharp.Quotations.Patterns
open System.Collections

let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance

let inline arguExn fmt = Printf.ksprintf(fun msg -> raise <| ArguException(msg)) fmt

let inline arguExnChain exn fmt = Printf.ksprintf(fun msg -> raise <| ArguException(msg, exn)) fmt

/// get CL arguments from environment
let getEnvironmentCommandLineArgs () =
    match Environment.GetCommandLineArgs() with
    | [||] -> [||]
    | args -> args[1..]

[<RequireQualifiedAccess>]
module Seq =

    /// partition sequence according to predicate; materialises both branches into arrays
    let partition (predicate : 'T -> bool) (ts : seq<'T>) =
        let l,r = new ResizeArray<'T>(), new ResizeArray<'T>()
        for t in ts do
            if predicate t then l.Add t else r.Add t

        l.ToArray(), r.ToArray()

[<RequireQualifiedAccess>]
module String =
    let inline mkWhiteSpace (length : int) = String(' ', length)
    let trimWithEllipsis len (s: string) = if s.Length > len then s.Substring(0, len) + "…" else s

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
    override _.Type = typeof<'T>
    override _.Accept func = func.Invoke<'T>()

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
    override _.Type = typeof<'Template>
    override _.Accept func = func.Invoke<'Template>()

and ITemplateFunc<'R> =
    abstract Invoke<'Template when 'Template :> IArgParserTemplate> : unit -> 'R

/// reflected version of Unchecked.defaultof
type Unchecked =
    static member UntypedDefaultOf(t : Type) =
        Existential.FromType(t).Accept {
            new IFunc<obj> with
                member _.Invoke<'T> () = Unchecked.defaultof<'T> :> obj
            }

type MemberInfo with
    member m.ContainsAttribute<'T when 'T :> Attribute> () : bool=
        m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

    /// Returns the matching attribute of type 'T, or None.
    /// When the member has multiple matching attributes (including any inherited
    /// from declaring types), the *last* attribute in reflection order is returned.
    /// Callers relying on inherited declaring-type attributes via `hasAttribute2`
    /// should be aware that an attribute on the case can be overridden by a later
    /// attribute on the same case, not by an attribute on the declaring type.
    member m.TryGetAttribute<'T when 'T :> Attribute> () : 'T option =
        match m.GetCustomAttributes(typeof<'T>, true) |> Seq.toArray with
        | [||] -> None
        | attrs -> attrs |> Array.last :?> 'T |> Some

type IDictionary<'K,'V> with
    member d.TryFind k =
        let ok,found = d.TryGetValue k
        if ok then Some found
        else None

// NOTE: System.Diagnostics.Process.GetCurrentProcess() (used pre 6.2.2)
//       yields `dotnet` when you `dotnet run` a program, or you invoke via `dotnet tool run
//       and/or if your IDE wraps the invocation etc
let currentProgramName () =
    match Assembly.GetEntryAssembly() with
    // GetEntryAssembly() returns null in unmanaged hosts, some test runners, and
    // hosting models without a CLR-defined entry point; fall back to the
    // AppDomain friendly name (always populated) rather than NRE.
    | null -> AppDomain.CurrentDomain.FriendlyName
    | a -> a.GetName().Name

/// recognize exprs that strictly contain DU constructors
/// e.g. <@ Case @> is valid but <@ fun x y -> Case y x @> is invalid
let expr2Uci (e : Expr) =
    let originalExprExcerpt () = sprintf "%A" e |> String.trimWithEllipsis 200
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
        | _ -> invalidArg (nameof e) (sprintf "Only union constructors are permitted in expression based queries. Got: %s" <| originalExprExcerpt ())

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
                        let nextCharAfterBackslashes = value |> Seq.skip (i + 1) |> Seq.filter (fun c -> c <> '\\') |> Seq.tryHead
                        if nextCharAfterBackslashes = Some '"' || nextCharAfterBackslashes = None then
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
    member _.Zero () : StringExpr<unit> = ignore
    member _.Bind(f : StringExpr<'T>, g : 'T -> StringExpr<'S>) : StringExpr<'S> =
        fun sb -> g (f sb) sb

    member _.Yield (txt : string) : StringExpr<unit> = fun b -> b.Append txt |> ignore
    member _.Yield (c : char) : StringExpr<unit> = fun b -> b.Append c |> ignore
    member _.YieldFrom (f : StringExpr<unit>) = f

    member _.Combine(f : StringExpr<unit>, g : StringExpr<'T>) : StringExpr<'T> = fun b -> f b; g b
    member _.Delay (f : unit -> StringExpr<'T>) : StringExpr<'T> = fun b -> f () b

    member _.For (xs : 'a seq, f : 'a -> StringExpr<unit>) : StringExpr<unit> =
        fun b ->
            use e = xs.GetEnumerator ()
            while e.MoveNext() do f e.Current b

    member _.While (p : unit -> bool, f : StringExpr<unit>) : StringExpr<unit> =
        fun b -> while p () do f b

let stringExpr = StringExprBuilder()

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringExpr =
    let build (f : StringExpr<unit>) =
        let b = StringBuilder()
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
    member x.Item(key : string) =
        let mutable kr = null
        let mutable vr = Unchecked.defaultof<_>
        if x.TryGetPrefix(key, &kr, &vr) && kr = key then vr
        else
            raise <| KeyNotFoundException(key)

    /// Look up best matching key entry by prefix
    member _.TryGetPrefix(value : string, kresult : byref<string>, vresult : byref<'Value>) : bool =
        // Just iterate through all the keys, picking the matching prefix
        // with the maximal length
        let mutable maxPos = -1
        let mutable maxLen = -1
        for i = 0 to keys.Length - 1 do
            let key = keys[i]
            let pLength =
                if value.StartsWith(key, StringComparison.Ordinal) then key.Length
                else -1

            if pLength > maxLen then
                maxPos <- i
                maxLen <- pLength

        if maxPos < 0 then false
        else kresult <- keys[maxPos] ; vresult <- values[maxPos] ; true

    member _.Count with get(): int = keys.Length
    interface IEnumerable<string * 'Value> with
        member _.GetEnumerator() = keyVals.GetEnumerator()
        member _.GetEnumerator() = keyVals.GetEnumerator() :> IEnumerator

/// Gets the default width of the current console window,
/// if available. Falls back to 80 when stdout is redirected,
/// when the host has no console (e.g. some IDE test runners),
/// or when the detected width is smaller than 80.
let getDefaultCharacterWidth() =
    let detected =
        try Console.WindowWidth - 1
        with
        | :? System.IO.IOException -> 80
        | :? PlatformNotSupportedException -> 80
        | _ -> 80
    max detected 80

// Wordwrap implementation kindly provided by @forki
let wordwrap (width:int) (inputText:string) =
    let breakLine (text:string) (pos:int) (max:int) =
        // Find last whitespace in line
        let mutable i = max - 1
        while i >= 0 && not (Char.IsWhiteSpace text[pos + i]) do
            i <- i - 1

        if i < 0 then
            max // No whitespace found; break at maximum length
        else
            // Find start of whitespace
            while i >= 0 && Char.IsWhiteSpace text[pos + i] do
                i <- i - 1
            // Return length of text before whitespace
            i + 1

    if width < 1 then invalidArg "width" "Must be positive number."

    let inputText = inputText.Replace("\r\n","\n").Replace("\r","\n")
    let lines = ResizeArray<string>()
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

                inputText[pos .. pos + len - 1] |> lines.Add

                // Trim whitespace following break
                pos <- pos + len

                while pos < eol && Char.IsWhiteSpace inputText[pos] do
                    pos <- pos + 1

        pos <- next

    lines.ToArray() |> Array.toList
