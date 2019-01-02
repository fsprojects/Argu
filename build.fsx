// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"

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

let summary = "A declarative command line and XML configuration parser for F# applications."

let gitOwner = "fsprojects"
let gitName = "Argu"
let gitHome = "https://github.com/" + gitOwner
let gitRaw = "https://raw.github.com/" + gitOwner

let testProjects = "tests/**/*.??proj"

let configuration = environVarOrDefault "Configuration" "Release"
let artifacts = __SOURCE_DIRECTORY__ @@ "artifacts"
let binFolder = __SOURCE_DIRECTORY__ @@ "bin" @@ configuration

//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" release.NugetVersion) |> ignore
)

Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title projectName
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ()
        )
)


// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs [ binFolder ; artifacts ]
)

//
//// --------------------------------------------------------------------------------------
//// Build library & test project

Target "Build" (fun _ ->
    DotNetCli.Build (fun p ->
        { p with
            Project = __SOURCE_DIRECTORY__ 
            Configuration = configuration })
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let runTests config (proj : string) =
    if EnvironmentHelper.isWindows then
        DotNetCli.Test (fun c ->
            { c with
                Project = proj
                Configuration = config })
    else
        // work around xunit/mono issue
        let projDir = Path.GetDirectoryName proj
        let projName = Path.GetFileNameWithoutExtension proj
        let netcoreFrameworks, legacyFrameworks = 
            !! (projDir @@ "bin" @@ config @@ "*/")
            |> Seq.map Path.GetFileName
            |> Seq.toArray
            |> Array.partition 
                (fun f -> 
                    f.StartsWith "netcore" || 
                    f.StartsWith "netstandard")

        for framework in netcoreFrameworks do
            DotNetCli.Test (fun c ->
                { c with
                    Project = proj
                    Framework = framework
                    Configuration = config })

        for framework in legacyFrameworks do
            let assembly = projDir @@ "bin" @@ config @@ framework @@ projName + ".dll"
            !! assembly
            |> xUnit2 (fun c ->
                { c with
                    Parallel = ParallelMode.Collections
                    TimeOut = TimeSpan.FromMinutes 20. })

Target "RunTests" (fun _ ->
    for proj in !! testProjects do
        runTests "Release" proj)

//
//// --------------------------------------------------------------------------------------
//// Build a NuGet package

Target "NuGet.Pack" (fun _ ->
    Paket.Pack(fun config ->
        { config with 
            Version = release.NugetVersion
            ReleaseNotes = String.concat Environment.NewLine release.Notes
            OutputPath = artifacts
        }))

Target "NuGet.Pack.SDK" (fun _ ->
    DotNetCli.Pack(fun p ->
        { p with
            OutputPath = artifacts
            Configuration = configuration
            Project = !! "**/Argu.fsproj" |> Seq.head
            AdditionalArgs =
                [ yield "--no-build" ;
                  yield "--no-restore" ;
                  yield sprintf "-p:Version=%s" release.NugetVersion ]
            })
)

Target "Sourcelink.Test" (fun _ ->
    !! (sprintf "%s/*.nupkg" artifacts)
    |> Seq.iter (fun nupkg ->
        DotNetCli.RunCommand
            (fun p -> { p with WorkingDir = __SOURCE_DIRECTORY__ @@ "tests" @@ "Argu.Tests" } )
            (sprintf "sourcelink test %s" nupkg)
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
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  ==> "NuGet.Pack"
  ==> "Sourcelink.Test"
  ==> "GenerateDocs"
  ==> "Bundle"

"Bundle"
  ==> "ReleaseDocs"
  ==> "ReleaseGitHub"
  ==> "NuGet.Push"
  ==> "Release"

RunTargetOrDefault "Default"