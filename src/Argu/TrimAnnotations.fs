// Polyfills for AOT/trim diagnostic attributes so that
// netstandard2.0 builds can carry the same metadata as .NET 7+ targets.
// The .NET trimmer / publish-AOT pipeline matches by full type name,
// not by assembly, so internal copies are picked up.
namespace System.Diagnostics.CodeAnalysis

open System

#if !NET7_0_OR_GREATER

/// <summary>
///     Marker indicating that the annotated method requires dynamic-code
///     generation at runtime (e.g. <c>MakeGenericType</c> +
///     <c>Activator.CreateInstance</c>) and is therefore not safe under
///     publish-AOT. Callers that opt into AOT will see an <c>IL3050</c>
///     warning when invoking the annotated member.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Constructor ||| AttributeTargets.Class, AllowMultiple = false, Inherited = false)>]
type internal RequiresDynamicCodeAttribute(message : string) =
    inherit Attribute()
    /// Reason the method requires dynamic code.
    member _.Message = message
    /// Optional URL pointing at extended documentation.
    member val Url : string = null with get, set

/// <summary>
///     Marker indicating that the annotated method uses reflection in a
///     way the trimmer cannot statically analyse (e.g. arbitrary type
///     reads via <c>FSharpType.GetUnionCases</c>). Callers that enable
///     trimming will see an <c>IL2026</c> warning.
/// </summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Constructor ||| AttributeTargets.Class, AllowMultiple = false, Inherited = false)>]
type internal RequiresUnreferencedCodeAttribute(message : string) =
    inherit Attribute()
    /// Reason the method holds unreferenced-code requirements.
    member _.Message = message
    /// Optional URL pointing at extended documentation.
    member val Url : string = null with get, set

#endif
