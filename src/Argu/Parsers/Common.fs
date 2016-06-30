[<AutoOpen>]
module internal Argu.CommonParsers

exception ParseError of message:string * code:ErrorCode * argInfo:UnionArgInfo
exception HelpText of subcommand:UnionArgInfo

let inline error argInfo code fmt =
    Printf.ksprintf (fun msg -> raise <| ParseError("ERROR: " + msg, code, argInfo)) fmt

/// construct a parse result from untyped collection of parsed arguments
let mkUnionCase (info : UnionCaseArgInfo) index parseSource parsecontext (fields : obj []) =
    {
        Value = info.CaseCtor fields
        Index = index
        FieldContents =
            match info.FieldCtor.Value with
            | None -> null
            | Some ctor -> ctor fields

        ArgInfo = info
        Source = parseSource
        ParseContext = parsecontext
    }

/// Create a ParseResult<_> instance from a set of template parameters
let mkParseResultFromValues (info : UnionArgInfo) (exiter : IExiter) (width : int)
                            (programName : string) (description : string option) 
                            (values : seq<'Template>) =

    let agg = info.Cases |> Array.map (fun _ -> new ResizeArray<UnionCaseParseResult>())
    values |> Seq.iteri (fun i value ->
        let value = value :> obj
        let tag = info.TagReader.Value value
        let case = info.Cases.[tag]
        let fields = case.FieldReader.Value value
        let result = mkUnionCase case i ParseSource.None case.Name fields
        agg.[tag].Add result)

    let results = 
        { 
            IsUsageRequested = false
            UnrecognizedCliParams = []
            Cases = agg |> Array.map (fun rs -> rs.ToArray())
        }

    new ParseResult<'Template>(info, results, programName, description, width, exiter)