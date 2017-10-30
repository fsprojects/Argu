// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"
#load "packages/build/SourceLink.Fake/tools/SourceLink.fsx"

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
let gitRaw = "https://raw.github.com/" + gitOwner

let testAssemblies = !! "bin/Argu.Tests.dll"

let netcoreSrcFiles = !! "src/**/project.json" |> Seq.toList
let netcoreTestFiles = !! "tests/**/project.json" |> Seq.toList

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
    testAssemblies
    |> xUnit2 (fun p ->
        { p with
            Parallel = ParallelMode.Collections
            TimeOut = TimeSpan.FromMinutes 20. })
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

//----------------------------
// SourceLink

open SourceLink

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw project
    [ yield! !! "src/**/*.??proj" ]
    |> Seq.iter (fun projFile ->
        let proj = VsProj.LoadRelease projFile
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl
    )
)

// Github Releases
#nowarn "85"
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
    for proj in netcoreSrcFiles @ netcoreTestFiles do
        DotNetCli.SetVersionInProjectJson release.NugetVersion proj
)

Target "Build.NetCore" (fun _ ->
    for proj in netcoreSrcFiles @ netcoreTestFiles do
        DotNetCli.Restore (fun c -> { c with Project = proj })

    for proj in netcoreSrcFiles @ netcoreTestFiles do
        DotNetCli.Build (fun c -> { c with Project = proj })
)

Target "RunTests.NetCore" (fun _ ->
    for proj in netcoreTestFiles do
        DotNetCli.Test (fun c -> { c with Project = proj })
)

let isDotnetSDKInstalled = DotNetCli.isInstalled()

Target "NuGet.AddNetCore" (fun _ ->
    if not isDotnetSDKInstalled then failwith "You need to install .NET core to publish NuGet packages"
    for proj in netcoreSrcFiles do
        DotNetCli.Pack (fun c -> { c with Project = proj })

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
  //==> "SetVersionInProjectJSON"
  ==> "Prepare"
  ==> "Build.Net40"
  ==> "RunTests"
  ==> "Default"

"Default"
  //==> "PrepareRelease"
  //=?> ("Build.NetCore", isDotnetSDKInstalled)
  //=?> ("RunTests.NetCore", isDotnetSDKInstalled)
  ==> "NuGet.Pack"
  //==> "NuGet.AddNetCore"
  ==> "NuGet"
  ==> "GenerateDocs"
  ==> "SourceLink"
  ==> "ReleaseDocs"
  ==> "NuGetPush"
  ==> "ReleaseGitHub"
  ==> "Release"

RunTargetOrDefault "Default"