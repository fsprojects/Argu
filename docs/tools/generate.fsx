// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceProjects = [ "../../src/Argu/" ]

// Web site location for the generated documentation
let website = "/Argu"

let githubLink = "http://github.com/fsprojects/Argu"

// Specify more information about your project
let info =
  [ "project-name", "Argu"
    "project-author", "Eirik Tsarpalis"
    "project-summary", "A declarative argument parser for F#"
    "project-github", githubLink
    "project-nuget", "http://www.nuget.org/packages/Argu" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#I "../../packages/build/FSharp.Compiler.Service/lib/net45"
#I "../../packages/build/FSharp.Formatting/lib/net461"
#r "../../packages/build/FAKE/tools/FakeLib.dll"
#r "RazorEngine.dll"
#r "FSharp.Markdown.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.Formatting.Common.dll"
#r "FSharp.Formatting.Razor.dll"

open Fake
open System.IO
open FSharp.Formatting.Razor

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Paths with template/source/output locations
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/build/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
    [   templates; formatting @@ "templates"
        formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
    CopyRecursive files output true |> Log "Copying file: "
    ensureDirectory (output @@ "content")
    CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "


let getReferenceAssembliesForProject (proj : string) =
    !! (proj @@ "bin/Release/net4*/*.XML")
    |> Seq.map (fun p -> Path.ChangeExtension(p, "dll"))
    |> Seq.toList

// Build API reference from XML comments
let buildReference () =
    CleanDir (output @@ "reference")
    let binaries = referenceProjects |> List.collect getReferenceAssembliesForProject
    RazorMetadataFormat.Generate
        ( binaries, output @@ "reference", layoutRoots, 
          parameters = ("root", root)::info,
          sourceRepo = githubLink @@ "tree/master",
          sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
          publicOnly = true )

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
    let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
    let dirs = Seq.append [content] subdirs |> Seq.toList
    for dir in dirs do
        let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
        RazorLiterate.ProcessDirectory
            ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
            layoutRoots = layoutRoots, generateAnchors = true )

// Generate
CleanDir output
CreateDir output
copyFiles()
buildDocumentation()
buildReference()