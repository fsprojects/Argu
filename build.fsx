// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"
#r @"packages/build/FAKE/tools/Newtonsoft.Json.dll"

open System
open System.IO
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
open Fake.Testing

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "Argu"

let gitOwner = "fsprojects"
let gitName = "Argu"
let gitHome = "https://github.com/" + gitOwner

let testAssemblies = !! "bin/net40/Argu.Tests.dll"

//
//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")
let nugetVersion = release.NugetVersion

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" nugetVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fileName = "./src/Argu/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 
)


// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs ["./bin/"]
)

//
//// --------------------------------------------------------------------------------------
//// Build library & test project

let configuration = environVarOrDefault "Configuration" "Release"

let isTravisCI = (environVarOrDefault "TRAVIS" "") = "true"

Target "Build.Net35" (fun _ ->
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", "Release-NET35" ]
    |> Log "AppBuild-Output: "
)

Target "Build.Net40" (fun _ ->
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "AppBuild-Output: "
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target "RunTests" (fun _ ->
    ActivateFinalTarget "CloseTestRunner"

    testAssemblies
    |> xUnit2 (fun p ->
        { p with
            Parallel = ParallelMode.Collections
            TimeOut = TimeSpan.FromMinutes 20. })
)

FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)

//
//// --------------------------------------------------------------------------------------
//// Build a NuGet package

Target "NuGet" DoNothing

Target "NuGet.Pack" (fun _ ->
    Paket.Pack(fun config ->
        { config with 
            Version = release.NugetVersion
            ReleaseNotes = String.concat "\n" release.Notes
            OutputPath = "bin"
        }))

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin/" }))

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

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
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

// .NET Core SDK and .NET Core

let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x

Target "SetVersionInProjectJSON" (fun _ ->
    !! "./**/project.json"
    |> Seq.iter (DotNet.SetVersionInProjectJson release.NugetVersion)
)

Target "Build.NetCore" (fun _ ->
    DotNet.Restore id

    !! "src/**/project.json"
    |> DotNet.Build id
)

Target "RunTests.NetCore" (fun _ ->
    !! "tests/**/project.json"
    |> DotNet.Test id
)

let isDotnetSDKInstalled = DotNet.isInstalled()

Target "NuGet.AddNetCore" (fun _ ->
    if not isDotnetSDKInstalled then failwith "You need to install .NET core to publish NuGet packages"
    !! "src/**/project.json"
    |> DotNet.Pack id

    let nupkg = sprintf "../../bin/Argu.%s.nupkg" (release.NugetVersion)
    let netcoreNupkg = sprintf "bin/Release/Argu.%s.nupkg" (release.NugetVersion)

    Shell.Exec("dotnet", sprintf """mergenupkg --source "%s" --other "%s" --framework netstandard1.6 """ nupkg netcoreNupkg, "src/Argu/") |> assertExitCodeZero
)


Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "Default" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "SetVersionInProjectJSON"
  ==> "Prepare"
  ==> "Build.Net40"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  =?> ("Build.Net35", not isTravisCI) //mono 4.x doesnt have FSharp.Core 2.3.0.0 installed
  =?> ("Build.NetCore", isDotnetSDKInstalled)
  =?> ("RunTests.NetCore", isDotnetSDKInstalled)
  ==> "NuGet.Pack"
  ==> "NuGet.AddNetCore"
  ==> "NuGet"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "NuGetPush"
  ==> "ReleaseGitHub"
  ==> "Release"

RunTargetOrDefault "Default"