### 3.0.0-alpha007
* Add subcommand support.
* Add support for list and option parameters.
* Add support for grouped switches.
* Extend EqualsAssignment attribute for parameters of arity 2.
* Implement UniqueAttribute and ExactlyOnceAttribute.
* Implement HelpFlagsAttribute, HelpDescriptionAttribute and DisableHelpAttribute.
* Add ParseResult.UnrecognizedCliParams property.

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