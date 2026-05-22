#nowarn "44" // ParseCSV and Rest attributes are intentionally [<Obsolete>]; we still parse them for backwards compatibility
module internal Argu.ObsoleteHelpers

let hasParseCsvAttribute (attributes: obj[]) =
    attributes |> Array.exists (fun x -> x :? ParseCSVAttribute)

let hasRestAttribute (attributes: obj[]) =
    attributes |> Array.exists (fun x -> x :? RestAttribute)
