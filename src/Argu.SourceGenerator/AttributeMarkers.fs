namespace Argu.SourceGenerator

open System

/// <summary>
///     Marker attribute used by the (planned) Argu source generator.
///     Apply to a CLI template discriminated union to opt that type into
///     compile-time schema generation. The companion generator (shipped
///     separately) reads this attribute and emits an
///     <c>ArgumentParser&lt;'T&gt;</c> factory that does not require
///     reflection at runtime — required for publish-AOT scenarios.
/// </summary>
/// <remarks>
///     This release ships the marker only. The generator that consumes
///     it is intentionally out of scope for the initial scaffold; the
///     marker is published so that downstream code can be annotated
///     ahead of the generator landing.
/// </remarks>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)>]
type ArguGenerateAttribute() =
    inherit Attribute()
