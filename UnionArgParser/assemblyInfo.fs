namespace UnionArgParser

    open System.Reflection

#if BUILD_STRONG_NAME
    [<assembly:AssemblyKeyFile("../../Lib/key.snk")>]
#endif
    [<assembly:AssemblyVersion("0.5.8.*")>]
    do()