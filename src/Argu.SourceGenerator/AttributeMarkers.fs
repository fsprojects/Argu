namespace Argu.SourceGenerator

open System

/// <summary>
///     Opts an Argu CLI template into compile-time schema generation: its
///     <c>ArgumentParser&lt;'T&gt;</c> is then built without runtime reflection,
///     which is what makes publish-AOT viable and removes the reflection cost
///     from startup.
///
///     Apply to a discriminated union that implements <c>IArgParserTemplate</c>.
///     The union's cases and their Argu attributes must be resolvable at compile
///     time — the generator reads the type statically, so a template whose shape
///     is built or varied at runtime cannot be generated for.
/// </summary>
/// <remarks>
///     The generator that consumes this attribute does not exist yet. This
///     package currently ships the marker alone, so applying it has no build-time
///     effect beyond recording intent. Code annotated now gains reflection-free
///     parsing once the generator lands, with no further source changes.
/// </remarks>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)>]
type ArguGenerateAttribute() =
    inherit Attribute()
