System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

// FAKE build script 
// --------------------------------------------------------------------------------------

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

// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "Argu"
let gitOwner = "fsprojects"
let gitName = "Argu"
let gitHome = "https://github.com/" + gitOwner
let gitRaw = "https://raw.github.com/" + gitOwner

let buildDir = __SOURCE_DIRECTORY__ </> "bin"
let pkgDir= __SOURCE_DIRECTORY__ </> "nupkg"

// The rest of the code is standard F# build script 
// --------------------------------------------------------------------------------------

// Read release notes & version info from RELEASE_NOTES.md
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


let configuration = environVarOrDefault "Configuration" "Release"
let isTravisCI = environVarOrDefault "TRAVIS" "false" = "true"

// Clean build results & restore NuGet packages
// --------------------------------------------------------------------------------------

Target "Clean" (fun _ ->
    if isTravisCI then traceImportant "Skip cleaning on Travis to avoid access violation errors" else
    !! "src/**/obj/"
    ++ "src/**/bin/"
    ++ "tests/**/obj/"
    ++ "tests/**/bin/"
    ++ "samples/obj/"
    ++ "samples/bin/"
    ++ "tools/obj/"
    ++ "tools/bin/"
    ++ "/**/obj/"
    ++ "/**/bin/"
    ++ "/bin/"
    ++ "/nupkg/"
    |> CleanDirs 
    ["/bin/";"/nupkg/"] |> Seq.iter ensureDirectory
)


// Build library & test project
// --------------------------------------------------------------------------------------

Target "BuildNetFramework" (fun _ ->
    [ project + ".sln" ]
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "AppBuild-Output: "
)

Target "BuildNet40" (fun _ ->
    ["src/Argu/Argu.fsproj" ]
    |> MSBuild "" "Build" ["Configuration", "Release-NET40" ]
    |> Log "AppBuild-Output: "
)

// Run the unit tests using test runner & kill test runner when complete
// --------------------------------------------------------------------------------------

let testAssemblies = !! "bin/tests/Argu.Tests.dll"

Target "RunTests" (fun _ ->
    testAssemblies |> NUnit3  (fun p ->
    { p with
        ShadowCopy = false
        TimeOut = TimeSpan.FromMinutes 20. }))

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


// SourceLink
//----------------------------

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
        |> Seq.filter (fun (s: string) -> s.EndsWith "(push)")
        |> Seq.tryFind (fun (s: string) -> s.Contains (gitOwner + "/" + gitName))
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


let dotnetcliVersion = "1.0.1"
let dotnetSDKPath = System.Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData </> "dotnetcore" </> dotnetcliVersion |> FullName
let dotnetExePath = dotnetSDKPath </> (if isWindows then "dotnet.exe" else "dotnet") |> FullName

// .NET Core SDK and .NET Core

let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x

let runCmdIn workDir exe = 
    Printf.ksprintf (fun args -> 
        tracefn "%s %s" exe args
        Shell.Exec (exe, args, workDir) |> assertExitCodeZero)

/// Execute a dotnet cli command
let dotnet workDir = runCmdIn workDir "dotnet"


Target "InstallDotNetCore" (fun _ ->
    let correctVersionInstalled = 
        try if FileInfo(dotnetExePath |> FullName).Exists then
                let processResult = 
                    ExecProcessAndReturnMessages (fun info ->  
                    info.FileName <- dotnetExePath
                    info.WorkingDirectory <- __SOURCE_DIRECTORY__
                    info.Arguments <- "--version") (TimeSpan.FromMinutes 30.)
                processResult.Messages |> separated "" = dotnetcliVersion
            else false
        with  _ -> false

    if correctVersionInstalled then
        tracefn "dotnetcli %s already installed" dotnetcliVersion
    else
        CleanDir dotnetSDKPath
        let archiveFileName = 
            if isLinux then sprintf "dotnet-dev-ubuntu-x64.%s.tar.gz" dotnetcliVersion
            elif Fake.EnvironmentHelper.isMacOS then sprintf "dotnet-dev-osx-x64.%s.tar.gz" dotnetcliVersion
            else sprintf "dotnet-dev-win-x64.%s.zip" dotnetcliVersion
        let downloadPath = sprintf "https://dotnetcli.azureedge.net/dotnet/Sdk/%s/%s" dotnetcliVersion archiveFileName
        let localPath = Path.Combine(dotnetSDKPath, archiveFileName)

        tracefn "Installing '%s' to '%s'" downloadPath localPath
        
        use webclient = new Net.WebClient ()
        webclient.DownloadFile (downloadPath, localPath)

        if isLinux || isMacOS then
            let assertExitCodeZero x =
                if x = 0 then () else
                failwithf "Command failed with exit code %i" x

            Shell.Exec ("tar", sprintf """-xvf "%s" -C "%s" """ localPath dotnetSDKPath)
            |> assertExitCodeZero
        else  
            System.IO.Compression.ZipFile.ExtractToDirectory(localPath, dotnetSDKPath)
        
        tracefn "dotnet cli path - %s" dotnetSDKPath
        System.IO.Directory.EnumerateFiles dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s" path)
        System.IO.Directory.EnumerateDirectories dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s%c" path System.IO.Path.DirectorySeparatorChar)

    let oldPath = System.Environment.GetEnvironmentVariable "PATH"
    System.Environment.SetEnvironmentVariable("PATH", sprintf "%s%s%s" dotnetSDKPath (System.IO.Path.PathSeparator.ToString()) oldPath)
)

let netcoreSrcFiles =  [ __SOURCE_DIRECTORY__  </> "src/Argu.netcore/Argu.netcore.fsproj"]
let netcoreTestFiles = [ __SOURCE_DIRECTORY__  </> "tests/Argu.Tests.netcore/Argu.Tests.netcore.fsproj"]

let dotnetRestore files =
    files |> Seq.iter (fun proj -> DotNetCli.Restore (fun c ->
    { c with
        Project = proj
        ToolPath = dotnetExePath }))

let dotnetBuild proj =
    DotNetCli.Build (fun c ->
    { c with        
        Project = proj
        Output =  buildDir</>"netstandard1.6"
        ToolPath = dotnetExePath })


let netstandard16proj = __SOURCE_DIRECTORY__</>"src/Argu.netcore/Argu.netcore.fsproj" |> FullName

Target "DotnetRestore" (fun _ -> dotnetRestore netcoreSrcFiles)
Target "DotnetBuild" (fun _ -> dotnetBuild netstandard16proj)


// Build a NuGet package
// --------------------------------------------------------------------------------------

Target "NuGetPackage" (fun _ ->    
    if isTravisCI then traceImportant "skipping packaging on TravisCI" else // travis can't package properly, makes mdbs instead of pdbs
    Paket.Pack (fun p -> 
    { p with 
        TemplateFile = __SOURCE_DIRECTORY__ </> "src" </> "Argu" </> "paket.template"
        WorkingDir = __SOURCE_DIRECTORY__ 
        OutputPath = __SOURCE_DIRECTORY__ </> pkgDir 
        ToolPath = __SOURCE_DIRECTORY__ </> ".paket/paket.exe" 
        Version = release.NugetVersion
        ReleaseNotes = toLines release.Notes })
)

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = pkgDir }))

Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "PrepareRelease" DoNothing
Target "NetFramework" DoNothing
Target "DotNetCore" DoNothing
Target "Default" DoNothing


"Clean"
  ==> "AssemblyInfo"
  ==> "BuildNetFramework"
  ==> "RunTests"
  ==> "BuildNet40"
  ==> "NetFramework"  
  

"Clean"
  ==> "InstallDotNetCore"
  ==> "DotnetRestore"
  ==> "DotnetBuild"
  ==> "DotNetCore"  
  
"NetFramework"
  ==> "DotNetCore"
  ==> "NuGetPackage" 
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  ==> "SourceLink"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "NuGetPush"
  ==> "ReleaseGitHub"
  ==> "Release"

RunTargetOrDefault "Default"

