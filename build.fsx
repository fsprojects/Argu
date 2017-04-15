System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"
#r "System.IO.Compression.FileSystem"
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

let buildDir = "bin"
let tempDir = "temp"

let testAssemblies = !! "bin/v4.6.1/Argu.Tests.dll"

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
    |> MSBuild "" "Build" ["Configuration","Release-NET40"]
    |> Log "AppBuild-Output: "
)

Target "Build.Net461" (fun _ ->
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "AppBuild-Output: "
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete
open XUnit2
Target "RunTests" (fun _ ->
    ActivateFinalTarget "CloseTestRunner"
    testAssemblies
    |> xUnit2 (fun p ->
        { p with
            
            ToolPath = "./packages/xunit.runner.console/tools/xunit.console.exe"
            Parallel = ParallelMode.Collections
            TimeOut = TimeSpan.FromMinutes 20. 
        })
)

FinalTarget "CloseTestRunner" (fun _ ->  
    //ProcessHelper.killProcess "nunit-agent.exe"
    ProcessHelper.killProcess  "xunit.console.exe"
)


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



let dotnetcliVersion = "2.0.0-alpha-005165"

let dotnetSDKPath = System.Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) </> "dotnetcore" |> FullName

let dotnetExePath =
    dotnetSDKPath </> (if isWindows then "dotnet.exe" else "dotnet")
    |> FullName

let netcoreFiles = !! "src/**.preview?/*.fsproj" |> Seq.toList
// .NET Core SDK and .NET Core

let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x

let runCmdIn workDir exe = 
    Printf.ksprintf (fun args -> 
        tracefn "%s %s" exe args
        Shell.Exec(exe, args, workDir) |> assertExitCodeZero)

/// Execute a dotnet cli command
let dotnet workDir = runCmdIn workDir "dotnet"


Target "InstallDotNetCore" (fun _ ->
    let correctVersionInstalled = 
        try if FileInfo(dotnetExePath |> Path.GetFullPath).Exists then
                let processResult = 
                    ExecProcessAndReturnMessages (fun info ->  
                    info.FileName <- dotnetExePath
                    info.WorkingDirectory <- Environment.CurrentDirectory
                    info.Arguments <- "--version") (TimeSpan.FromMinutes 30.)

                processResult.Messages |> separated "" = dotnetcliVersion
            else false
        with  _ -> false

    if correctVersionInstalled then
        tracefn "dotnetcli %s already installed" dotnetcliVersion
    else
        CleanDir dotnetSDKPath
        let archiveFileName = 
            if isLinux then
                sprintf "dotnet-dev-ubuntu-x64.%s.tar.gz" dotnetcliVersion
            elif Fake.EnvironmentHelper.isMacOS then
                sprintf "dotnet-dev-osx-x64.%s.tar.gz" dotnetcliVersion
            else
                sprintf "dotnet-dev-win-x64.%s.zip" dotnetcliVersion
        let downloadPath = 
                sprintf "https://dotnetcli.azureedge.net/dotnet/Sdk/%s/%s" dotnetcliVersion archiveFileName
        let localPath = Path.Combine(dotnetSDKPath, archiveFileName)

        tracefn "Installing '%s' to '%s'" downloadPath localPath
        
        use webclient = new Net.WebClient()
        webclient.DownloadFile(downloadPath, localPath)

        if isLinux || isMacOS then
            let assertExitCodeZero x =
                if x = 0 then () else
                failwithf "Command failed with exit code %i" x

            Shell.Exec("tar", sprintf """-xvf "%s" -C "%s" """ localPath dotnetSDKPath)
            |> assertExitCodeZero
        else  
            System.IO.Compression.ZipFile.ExtractToDirectory(localPath, dotnetSDKPath)
        
        tracefn "dotnet cli path - %s" dotnetSDKPath
        System.IO.Directory.EnumerateFiles dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s" path)
        System.IO.Directory.EnumerateDirectories dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s%c" path System.IO.Path.DirectorySeparatorChar)

    let oldPath = System.Environment.GetEnvironmentVariable("PATH")
    System.Environment.SetEnvironmentVariable("PATH", sprintf "%s%s%s" dotnetSDKPath (System.IO.Path.PathSeparator.ToString()) oldPath)
)



Target "DotnetRestore" (fun _ ->
    netcoreFiles
    |> Seq.iter (fun proj ->
        DotNetCli.Restore (fun c ->
            { c with
                Project = proj
                ToolPath = dotnetExePath
            })
    )
)

Target "DotnetBuild" (fun _ ->
    netcoreFiles
    |> Seq.iter (fun proj ->
        DotNetCli.Build (fun c ->
            { c with
                Project = proj
                ToolPath = dotnetExePath
            })
    )
)

Target "DotnetPackage" (fun _ ->
    netcoreFiles
    |> Seq.iter (fun proj ->
        DotNetCli.Pack (fun c ->
            { c with
                Project = proj
                ToolPath = dotnetExePath
                AdditionalArgs = [(sprintf "-o %s" currentDirectory </> tempDir </> "dotnetcore"); (sprintf "/p:Version=%s" release.NugetVersion)]
            })
    )
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


//
//// --------------------------------------------------------------------------------------
//// Build a NuGet package

Target "NuGet" (fun _ ->    
    !! "integrationtests/**/paket.template" |> Seq.iter DeleteFile
    
    let files = !! "src/**/*.preview*" |> Seq.toList
    for file in files do
        File.Move(file,file + ".temp")

    Paket.Pack (fun p -> 
        { p with 
            ToolPath = "bin/merged/paket.exe" 
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes })

    for file in files do
        File.Move(file + ".temp",file)
)

Target "MergeDotnetCoreIntoNuget" (fun _ ->

    let nupkg = tempDir </> sprintf "Paket.Core.%s.nupkg" (release.NugetVersion) |> Path.GetFullPath
    let netcoreNupkg = tempDir </> "dotnetcore" </> sprintf "Paket.Core.%s.nupkg" (release.NugetVersion) |> Path.GetFullPath

    let runTool = runCmdIn "tools" dotnetExePath

    runTool """mergenupkg --source "%s" --other "%s" --framework netstandard1.6 """ nupkg netcoreNupkg
)


// Target "NuGet" DoNothing

// Target "NuGet.Pack" (fun _ ->
//     Paket.Pack(fun config ->
//         { config with 
//             Version = release.NugetVersion
//             ReleaseNotes = String.concat "\n" release.Notes
//             OutputPath = "bin"
//         }))

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin/" }))


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
  ==> "Build.Net461"
  ==> "RunTests"
  ==> "Default"

"Default"
  =?> ("Build.Net35", not isTravisCI) //mono 4.x doesnt have FSharp.Core 2.3.0.0 installed
  =?> ("Build.NetCore", isDotnetSDKInstalled)
  =?> ("RunTests.NetCore", isDotnetSDKInstalled)
  ==> "NuGet.Pack"
  ==> "NuGet.AddNetCore"
  ==> "NuGet"
  ==> "PrepareRelease"
  ==> "SourceLink"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "NuGetPush"
  ==> "ReleaseGitHub"
  ==> "Release"

RunTargetOrDefault "Default"