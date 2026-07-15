# Daily Diary

## 2026-06-10
**Docs Cleanup & Repo Hygiene**

- Removed stale ParseConfig reference from release notes to keep docs accurate.
- Merged latest master changes into async config reader branch to stay in sync.
- Added ignore patterns for bash, .NET, and IDE artifacts to keep repo clean.
## 2026-06-08
**AOT Compatibility & Async Config Reader**

- Trimmed .gitignore down to repo-specific entries, removing 341 lines of generic noise.
- Marked AOT-incompatible APIs with RequiresDynamicCode and RequiresUnreferencedCode attributes (#314) for native-AOT compatibility.
- Scaffolded a source generator project with an [ArguGenerate] marker attribute (#318) to support compile-time code generation.
- Finalized ConfigurationReader.FromEnvironmentVariables(prefix) and FromMicrosoftConfiguration (#308), iterating through several revisions to land the API.
- Merged upstream master and Ruben Bartelink's tidy-up and combined-commit changes into the async config reader branch, keeping the PR in sync.
## 2026-06-07
**Async Config Reader & Benchmarks**

- Redesigned IAsyncConfigurationReader around batched reads for efficiency
- Added Azure Key Vault sample, tutorial section, and failure-mode docs for async config
- Added BenchmarkDotNet harness for performance testing (#313)
- Cleaned up repo by removing .recode tooling artifacts and adding ignore rule
## 2026-06-05
**Attribute Cleanup & Config Reader Merge**

- Added unified SeparatorAttribute, marked 6 old separator attributes obsolete (#315)
- Fixed usage rendering bug when param description empty (#173, #323)
- Shipped Obsolete markers on deprecated PostProcess overloads (#296)
- Swapped PrefixDictionary internals to char-keyed trie for perf gain (#316)
- Merged master into async-config-reader branch, pulling in recent upstream changes
- Added F# build state to gitignore
## 2026-06-04
**ParseConfig Overload & Usage Strings**

- Added a record-based Parse(config) overload for ParseConfig, giving callers a way to invoke parsing with a configuration object instead of separate parameters (#307).
- Merged latest changes from origin/master, resolving conflicts across 22 files via git-sync.
- Introduced a UsageStrings record holding localisable labels for the usage/help message, laying groundwork for translating CLI output (#303).
## 2026-06-03
**Argu Parser Performance & Cleanup**

- Reverted premature .gitignore expansion (removed 341 lines) to keep repo config minimal
- Optimized ParseResults.getAllResults by replacing Seq pipeline with direct buffer access
- Flattened UnionCaseArgInfo fields (Name, CommandLineNames, AppSettingsName) for reduced overhead across 7 files
- Refactored Cli.parseCommandLinePartial into named helper functions for readability
- Fixed usage/help output to narrow the Console.WindowWidth exception catch and floor the wrap width calculation
## 2026-06-02
**F# Nullable Refs & Test Suite Split**

- Enabled F# nullable reference types on the Argu.fsproj to catch null-related bugs at compile time.
- Refactored UnParsers to use mutable closure state instead of workarounds, and fixed broken RELEASE_NOTES URLs.
- Split the monolithic Tests.fs file into three focused modules: Shared, Tests, and PrimitiveTests, improving test organization and maintainability.
## 2026-06-01
**Argu Introspection & Async Fixes**

- Fixed ParseAsync so faulted GetValueAsync treated as missing, matching sync parser behaviour
- Added Argu.Samples.Introspect sample demonstrating introspection API
- Fixed mandatory-group validation to report all missing groups in single error message instead of one at a time
- Refactored UnionCaseArgInfo to remove unneeded laziness, tidied parent backreferences
- Cleaned up repo tooling: removed .recode artifacts and added ignore rules
## 2026-05-30
```json
{
  "title": "Build Migration & Test Framework Cleanup",
  "bullets": [
    "Merged PR #08 adding null-guard checks on entry assembly resolution, plus a changelog correction.",
    "Migrated the test suite to xunit v3 running on Microsoft Testing Platform v2, cleaning up leftover test config in a follow-up commit.",
    "Converted the solution file to the new .slnx format, trimming 97 lines from the old .sln.",
    "Scoped an nowarn suppression to the specific case it was needed for (issue #289) instead of leaving it broad.",
    "Bumped DotNet.ReproducibleBuilds to 2.0.2.",
    "Clarified the exception message thrown by expr2Uci for easier debugging (issue #293).",
    "Added a build workaround to keep SDK 10 compatible with Microsoft Testing Platform v2 after the migration surfaced a break.",
    "Did general top-and-tail cleanup pass across the touched files."
  ]
}
```
## 2026-05-28
**Repo Hygiene: Gitignore Cleanup**

- Expanded .gitignore with build artifacts and IDE-specific patterns for bash, .NET, and common IDEs.
- Touched 3 files (1 added, 1 modified) to reduce untracked noise in future git status checks.
## 2026-05-27
**Docs Toolchain Bump**

- Bumped fsdocs-tool dependency from version 20.0.1 to 22.1.0 to keep the documentation generation toolchain current.
## 2026-05-25
**Argu Cleanup & Docs**

- Stripped reimplemented stdlib helpers from Utils.fs (#290), trimming 44 lines of dead weight.
- Added XML doc summaries to previously-undocumented public members (#304).
- Documented last-wins attribute pickup behavior in TryGetAttribute (#291).
- Documented order-sensitive equality on ParseResults (#295).
- Added 204 lines of coverage tests for error text, AppSettings, and deep subcommand scenarios (#312).
- Merged master into the async-config-reader branch to stay current.
## 2026-05-23
**Async Configuration Reader**

- Added IAsyncConfigurationReader interface plus ParseAsync overload for async config parsing
- Added test coverage for new IAsyncConfigurationReader and ParseAsync functionality
## 2026-05-22
**Program Name Resolution & Late-Binding Refactor**

- Added null-guard around Assembly.GetEntryAssembly() when deriving program name, preventing crash when entry assembly is unavailable (e.g. certain host/test environments)
- Refactored late-binding logic from ref-cell pattern to a mutable let binding, simplifying state handling across 14 added and 8 removed lines in the parser
