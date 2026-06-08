module Argu.Benchmarks.Program

open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs

[<EntryPoint>]
let main argv =
    // Pass through CLI args so users can do
    //   dotnet run -c Release --project benchmarks/Argu.Benchmarks -- --filter "*Parse*"
    // or `--list flat` / `--memory` etc.
    BenchmarkSwitcher.FromTypes(
        [| typeof<SchemaBuild>
           typeof<Parse>
           typeof<HelpRender> |])
        .Run(argv, DefaultConfig.Instance)
    |> ignore
    0
