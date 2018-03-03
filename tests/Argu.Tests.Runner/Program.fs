namespace Argu.Tests.Runner

open System
open MBrace.FsPickler
open System.IO
open System.Diagnostics

module Program =
    type private T = T
    let run(args) =
        let psi = ProcessStartInfo()
#if NET461
        let exe = typeof<T>.Assembly.Location
        psi.FileName <- exe
        psi.Arguments <- args
#else
        psi.FileName <- "dotnet"
        let dll =
            // this dll was copied to the bin of Argu.Core.Tests, but required files to actually run it are missing.
            // => go to the *original* /bin of Argu.Tests.Runner.
            let currentDll = typeof<T>.Assembly.Location
            let currentDir = Path.GetDirectoryName currentDll
#if DEBUG
            let configuration = "Debug"
#else
            let configuration = "Release"
#endif
            Path.Combine(currentDir, "..", "..", "..", "tests", "Argu.Tests.Runner", "bin", configuration, "netcoreapp2.0", Path.GetFileName currentDll)
        psi.Arguments <- "\"" + dll + "\" " + args
#endif
        psi.UseShellExecute <- false
        use p = Process.Start(psi)
        p.WaitForExit()
        if p.ExitCode <> 0 then
            failwithf "Error: runner exited with %i" p.ExitCode

    [<EntryPoint>]
    let private main argv =
        try
            let outputFile = Environment.GetEnvironmentVariable("_ARGU_TEST_OUTFILE")
            if String.IsNullOrWhiteSpace(outputFile) then
                failwith "out-file is unspecified"

            // I *could* invent a simple serialization format - or I could just use the trusted FsPickler
            let serializer = FsPickler.CreateBinarySerializer()
            let bytes = serializer.Pickle(argv)
            File.WriteAllBytes(outputFile, bytes)

            0
        with
          exn ->
            eprintfn "Error: %A" exn
            1
