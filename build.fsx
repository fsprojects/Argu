// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"

#nowarn "85"
#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"

open System
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let gitOwner = "fsprojects"
let gitName = "Argu"
let gitHome = "https://github.com/" + gitOwner
let gitRaw = "https://raw.github.com/" + gitOwner

let configuration = environVarOrDefault "Configuration" "Release"

let artifacts = __SOURCE_DIRECTORY__ @@ "artifacts"

//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" release.NugetVersion) |> ignore
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs [ artifacts ]
)

//
//// --------------------------------------------------------------------------------------
//// Build library & test project

Target "Build" (fun _ ->
    DotNetCli.Build (fun p ->
        { p with
            Project = __SOURCE_DIRECTORY__ 
            Configuration = configuration
            AdditionalArgs = 
                [
                    "-p:Version=" + release.NugetVersion
                    "-p:GenerateAssemblyInfo=true"
                ] 
        }
    )
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let testProjects = __SOURCE_DIRECTORY__ @@ "tests/**/*.??proj"

Target "RunTests" (fun _ ->
    for proj in !! testProjects do
        DotNetCli.Test (fun c ->
            { c with
                Project = proj
                Configuration = configuration
                AdditionalArgs = 
                    [
                        yield "--no-build"
                        yield "--"
                        if EnvironmentHelper.isMono then yield "RunConfiguration.DisableAppDomain=true"
                    ]
            }
        )
)


// --------------------------------------------------------------------------------------
// Build a NuGet package

let nugetProjects = !! "src/**/*.??proj"

Target "NuGet.Pack" (fun _ ->
    for proj in nugetProjects do
        DotNetCli.Pack(fun p ->
            { p with
                OutputPath = artifacts
                Configuration = configuration
                Project = proj
                AdditionalArgs =
                    [ 
                      "--no-build"
                      sprintf "-p:Version=%s" release.NugetVersion
                      sprintf "-p:PackageReleaseNotes=\"%s\"" (String.concat Environment.NewLine release.Notes) ]
            }
        )
)

Target "NuGet.Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = artifacts }))

// Doc generation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    let outputDocsDir = "docs/output"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    ensureDirectory outputDocsDir
    CopyRecursive outputDocsDir tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

// Github Releases
open Octokit

Target "ReleaseGitHub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    //StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let client =
        match Environment.GetEnvironmentVariable "OctokitToken" with
        | null -> 
            let user =
                match getBuildParam "github-user" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserInput "Username: "
            let pw =
                match getBuildParam "github-pw" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserPassword "Password: "

            createClient user pw
        | token -> createClientWithToken token

    // release on github
    client
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> releaseDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Root" DoNothing
Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "Default" DoNothing
Target "Bundle" DoNothing
Target "Release" DoNothing

"Root"
  ==> "Clean"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  ==> "NuGet.Pack"
  ==> "GenerateDocs"
  ==> "Bundle"

"Bundle"
  ==> "ReleaseDocs"
  ==> "ReleaseGitHub"
  ==> "NuGet.Push"
  ==> "Release"

RunTargetOrDefault "Default"