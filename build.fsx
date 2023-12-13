// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#r "nuget: System.Reactive        ,5.0.0"
#r "nuget: Fake.Core.UserInput    ,5.23.1"
#r "nuget: Fake.Core.ReleaseNotes ,5.23.1"
#r "nuget: Fake.Core.Target       ,5.23.1"
#r "nuget: Fake.IO.FileSystem     ,5.23.1"
#r "nuget: Fake.DotNet.Cli        ,5.23.1"
#r "nuget: Fake.Tools.Git         ,5.23.1"
#r "nuget: Fake.Api.Github        ,5.23.1"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open Fake.Api
open System

#if !FAKE
Environment.GetCommandLineArgs()
|> Array.skip 2
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext
#endif

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let gitOwner = "fsprojects"
let gitName = "Argu"
let gitHome = "https://github.com/" + gitOwner

let configuration = Environment.environVarOrDefault "Configuration" "Release"

let artifacts = __SOURCE_DIRECTORY__ @@ "artifacts"

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script 
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [ artifacts ]
)

//
// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    DotNet.build (fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString configuration

            MSBuildParams =
            { c.MSBuildParams with
                Properties = [("Version", release.NugetVersion)] }

        }) __SOURCE_DIRECTORY__
)


// ------------------
// Run the unit tests 

Target.create "RunTests" (fun _ ->
    DotNet.test (fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString configuration
            NoBuild = true
            Blame = true
        }) __SOURCE_DIRECTORY__
)


// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet.Pack" (fun _ ->
    let releaseNotes = String.toLines release.Notes |> System.Net.WebUtility.HtmlEncode
    DotNet.pack (fun pack ->
        { pack with
            OutputPath = Some artifacts
            Configuration = DotNet.BuildConfiguration.Release
            MSBuildParams =
                { pack.MSBuildParams with
                    Properties = 
                        [("Version", release.NugetVersion)
                         ("PackageReleaseNotes", releaseNotes)] }
        }) __SOURCE_DIRECTORY__
)

Target.create "NuGet.ValidateSourceLink" (fun _ ->
    for nupkg in !! (artifacts @@ "*.nupkg") do
        let p = DotNet.exec id "sourcelink" (sprintf "test %s" nupkg)
        if not p.OK then failwithf "failed to validate sourcelink for %s" nupkg
)

Target.create "NuGet.Push" (fun _ ->
    let source = "https://api.nuget.org/v3/index.json"
    let apikey =  Environment.environVarOrDefault "NUGET_KEY" ""
    for artifact in !! (artifacts + "/*nupkg") do
        let result = DotNet.exec id "nuget" (sprintf "push -s %s -k %s %s" source apikey artifact)
        if not result.OK then failwith "failed to push packages"
)

// Doc generation

Target.create "GenerateDocs" (fun _ ->
   Shell.cleanDir ".fsdocs"
   DotNet.exec id "fsdocs" "build --clean --strict --properties Configuration=Release" |> ignore
)

// Github Releases

Target.create "ReleaseGitHub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    //StageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    let client =
        match Environment.GetEnvironmentVariable "GITHUB_TOKEN" with
        | null -> 
            let user =
                match Environment.environVarOrDefault "github-user" "" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> UserInput.getUserInput "Username: "
            let pw =
                match Environment.environVarOrDefault "github-pw" "" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> UserInput.getUserInput "Password: "

            GitHub.createClient user pw
        | token -> GitHub.createClientWithToken token

    // release on github
    client
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "Root" ignore
Target.create "Prepare" ignore
Target.create "PrepareRelease" ignore
Target.create "Default" ignore
Target.create "Bundle" ignore
Target.create "Release" ignore

"Root"
  ==> "Clean"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  ==> "NuGet.Pack"
//   ==> "NuGet.ValidateSourceLink"
  ==> "GenerateDocs"
  ==> "Bundle"

"Bundle"
  ==> "GenerateDocs"
  ==> "ReleaseGitHub"
  ==> "NuGet.Push"
  ==> "Release"

Target.runOrDefault "Default"
