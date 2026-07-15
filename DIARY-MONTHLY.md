# Monthly Diary

## 2026-06
**Docs Cleanup & Repo Hygiene**

- Removed stale ParseConfig reference from release notes to keep docs accurate.
- Merged latest master changes into async config reader branch to stay in sync.
- Added ignore patterns for bash, .NET, and IDE artifacts to keep repo clean.
**AOT Compatibility & Async Config Reader**

- Trimmed .gitignore down to repo-specific entries, removing 341 lines of generic noise.
- Marked AOT-incompatible APIs with RequiresDynamicCode and RequiresUnreferencedCode attributes (#314) for native-AOT compatibility.
- Scaffolded a source generator project with an [ArguGenerate] marker attribute (#318) to support compile-time code generation.
- Finalized ConfigurationReader.FromEnvironmentVariables(prefix) and FromMicrosoftConfiguration (#308), iterating through several revisions to land the API.
- Merged upstream master and Ruben Bartelink's tidy-up and combined-commit changes into the async config reader branch, keeping the PR in sync.
**Async Config Reader & Benchmarks**

- Redesigned IAsyncConfigurationReader around batched reads for efficiency
- Added Azure Key Vault sample, tutorial section, and failure-mode docs for async config
- Added BenchmarkDotNet harness for performance testing (#313)
- Cleaned up repo by removing .recode tooling artifacts and adding ignore rule
**Attribute Cleanup & Config Reader Merge**

- Added unified SeparatorAttribute, marked 6 old separator attributes obsolete (#315)
- Fixed usage rendering bug when param description empty (#173, #323)
- Shipped Obsolete markers on deprecated PostProcess overloads (#296)
- Swapped PrefixDictionary internals to char-keyed trie for perf gain (#316)
- Merged master into async-config-reader branch, pulling in recent upstream changes
- Added F# build state to gitignore
**ParseConfig Overload & Usage Strings**

- Added a record-based Parse(config) overload for ParseConfig, giving callers a way to invoke parsing with a configuration object instead of separate parameters (#307).
- Merged latest changes from origin/master, resolving conflicts across 22 files via git-sync.
- Introduced a UsageStrings record holding localisable labels for the usage/help message, laying groundwork for translating CLI output (#303).
**Argu Parser Performance & Cleanup**

- Reverted premature .gitignore expansion (removed 341 lines) to keep repo config minimal
- Optimized ParseResults.getAllResults by replacing Seq pipeline with direct buffer access
- Flattened UnionCaseArgInfo fields (Name, CommandLineNames, AppSettingsName) for reduced overhead across 7 files
- Refactored Cli.parseCommandLinePartial into named helper functions for readability
- Fixed usage/help output to narrow the Console.WindowWidth exception catch and floor the wrap width calculation
**F# Nullable Refs & Test Suite Split**

- Enabled F# nullable reference types on the Argu.fsproj to catch null-related bugs at compile time.
- Refactored UnParsers to use mutable closure state instead of workarounds, and fixed broken RELEASE_NOTES URLs.
- Split the monolithic Tests.fs file into three focused modules: Shared, Tests, and PrimitiveTests, improving test organization and maintainability.
**Argu Introspection & Async Fixes**

- Fixed ParseAsync so faulted GetValueAsync treated as missing, matching sync parser behaviour
- Added Argu.Samples.Introspect sample demonstrating introspection API
- Fixed mandatory-group validation to report all missing groups in single error message instead of one at a time
- Refactored UnionCaseArgInfo to remove unneeded laziness, tidied parent backreferences
- Cleaned up repo tooling: removed .recode artifacts and added ignore rules
