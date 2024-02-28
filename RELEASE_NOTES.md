### 6.2.2
* Fix default `programName` when invoking via a wrapper such as `dotnet.exe` [#233](https://github.com/fsprojects/Argu/pull/233)

### 6.2.1
* Fix `ParseResults.ProgramName` - make it public (cut and paste error in [#229](https://github.com/fsprojects/Argu/pull/229)) [#231](https://github.com/fsprojects/Argu/pull/231)

### 6.2.0
* Add `ParseResults.ProgramName` [#229](https://github.com/fsprojects/Argu/pull/229)
* Add `ParseResults.GetResult(expr, 'Field -> 'R): 'R` as alias for `PostProcessResult`, `ParseResults.GetResults(expr, 'Field -> 'R): 'R list` as alias for `PostProcessResults`, `ParseResults.TryGetResult(expr, 'Field -> 'R): 'R option` as alias for `TryPostProcessResult` [#230](https://github.com/fsprojects/Argu/pull/230)
* Add `ParseResults.GetResult(expr, defThunk: unit -> 'Field, parse: 'Field -> 'R): 'R` and `ParseResults.GetResults(expr, def: 'Field, parse: 'Field -> 'R): 'R` that trap parse exceptions, mapping them to parse exit messages [#230](https://github.com/fsprojects/Argu/pull/230)

### 6.1.5
* Fix the regression of the [#127](https://github.com/fsprojects/Argu/pull/127) merged in 6.1.2 and fix Mandatory arguments in nested subcommands. [#220](https://github.com/fsprojects/Argu/issues/220) [@fpellet](https://github.com/fpellet)

### 6.1.4
* Fix: remove incorrect `ReproducibleBuilds` reference [introduced in `6.1.3`](https://github.com/fsprojects/Argu/pull/174) [#202](https://github.com/fsprojects/Argu/pull/202)

### 6.1.3 (Unlisted)
* Add ParseResults.GetResult(expr, unit -> 'T) helper for Catch [#187](https://github.com/fsprojects/Argu/pull/187)
* Use [`Dotnet.ReproducibleBuilds`](https://github.com/dotnet/reproducible-builds) [#174](https://github.com/fsprojects/Argu/pull/174)

### 6.1.2
* Fix Mandatory arguments in nested subcommands. [#116](https://github.com/fsprojects/Argu/issues/116) [@chestercodes](https://github.com/chestercodes) 
* Fix Consistent handling of numeric decimal separators using invariant culture. [#159](https://github.com/fsprojects/Argu/issues/159) [@stmax82](https://github.com/stmax82)

### 6.1.1
* Fix CustomAssignmentOrSpacedAttribute interop with optional fields.

### 6.1.0
* Add a CustomAssignmentOrSpacedAttribute for custom assignments that can also be space separated.

### 6.0.0
* Target netstandard2.0 only.

### 5.5.0
* Publish separate symbols package.

### 5.4.0
* Chain inner exceptions in ArguExceptions.

### 5.3.0
* case-insensitivity for command-line enums.
* Add SubCommand attribute for nullary subcommands.

### 5.2.0
* Fix SourceLink issues.

### 5.1.0
* Performance optimizations.
* Fixed incorrect commandline option generation in non-English locales.

### 5.0.1
* Fix framework constraint issue.

### 5.0.0
* Allow use of query API with implicit quotations
* Require FSharp.Core 4.0 for net40.
* Require FSharp.Core 4.3 for netstandard2.0.

### 4.2.1
* Fix packaging issue.

### 4.2.0
* Migrate to SourceLink 2.0

### 4.1.0
* Add AppSettings configuration reader support for NetStandard.

### 4.0.0
* Support NetStandard 2.0
* Add Environment Variable configuration reader.

### 3.6.1
* Fix default character width issue in console apps.

### 3.6.0
* Fix wordwrapping issue in usage strings.

### 3.5.0
* Add SourceLink Support.

### 3.4.0
* Assignment separator parsing bugfix.

### 3.3.0
* Support CoreCLR.

### 3.2.0
* Add `hideSyntax` option in PrintUsage() method.

### 3.1.0
* Add support for MainCommand attribute.

### 3.0.1
* Bugfix usage string rendering issue where description might span multiple lines.

### 3.0.0
* Add subcommand support.
* Add support for list and option parameters.
* Add support for grouped switches.
* Extend EqualsAssignment attribute for parameters of arity 2.
* Implement UniqueAttribute and ExactlyOnceAttribute.
* Implement HelpFlagsAttribute, HelpDescriptionAttribute and DisableHelpAttribute.
* Implement GatherUnrecognizedAttribute.
* Add ParseResult.UnrecognizedCliParams property.
* Add support for F# unions used as enumeration parameters.

### 2.1.0
* AltCommandLineAttribute supporting variadic parameters.

### 2.0.0
* Move to fsprojects, rename root namespace.

### 1.1.3
* Fixes to error message formatting.

### 1.1.2
* Bugfix #40.

### 1.1.1
* Support slashes in command line parameter names.

### 1.1.0
* Implement parseResult.Iter methods.

### 1.0.0
* Rename project to Argu.
* Move back to F# 3.0 runtime by default.

### 0.9.0
* Move to F# 3.1 runtime.

### 0.8.7
* Fix CLI parser bug.

### 0.8.6
* Allow inclusion of '--help' parameters before arguments marked with FirstAttribute.

### 0.8.5
* Add support for parameterized CLI prefixes in auto-generated parameter names. Minor fixes.

### 0.8.4
* Add support for CLI syntax printing.

### 0.8.3
* Add checks for conflicting parameter identifiers.

### 0.8.2
* Fix package issue.

### 0.8.1
* Fix reflection bug.

### 0.8.0
* Add support for ignored parameters.
* Add support for '--param=argument' CLI syntax.
* Update ParseSource API.
* Fix BASE64 encoding issue.

### 0.7.1
* Replace ArgParser public constructor with factory method.

### 0.7.0
* Add support for Base64 binary parsing.

### 0.6.6
* Fix packaging issue.

### 0.6.5
* Add support for F# 3.1 DU labels.

### 0.6.4
* Add support for .NET 3.5

### 0.6.3
* Fix bug where parsing crashes if no app.config file present.

### 0.6.2
* Remove BindingFlags parameter from Argu.

### 0.6.1
* Add support for multiple alternative command line names.

#### 0.6.0
* Update root namespace to Nessos.Argu
* Add support for application configuration in class libraries
* Change application configuration parsing API.

#### 0.5.9
* Fix packaging issue.

#### 0.5.8
* Include optional BindingFlags parameter. 
* Include support for all primitive types.
