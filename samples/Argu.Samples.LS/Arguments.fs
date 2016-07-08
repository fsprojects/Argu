namespace Argu.Samples.LS

open Argu

// This sample attempts to replicate the command line syntax found in
// the GNU coreutils ls command.

type Size =
    | B = 1
    | K = 2
    | M = 3
    | G = 4

type ColorWhen =
    | Never  = 0
    | Always = 1
    | Auto   = 2

type QuotingStyle =
    | Literal               = 1
    | Locale                = 2
    | Shell                 = 3
    | Shell_Always          = 4
    | Shell_Escape          = 5
    | Shell_Escape_Always   = 6
    | C                     = 7
    | Escape                = 8

[<CliPrefix(CliPrefix.DoubleDash)>]
[<NoAppSettings>]
type LsArguments =
    | [<MainCommand>] Files of FILES:string list
    | [<AltCommandLine("-a")>] All
    | [<AltCommandLine("-A")>] Almost_All
    | Author
    | [<AltCommandLine("-b")>] Escape
    | [<EqualsAssignment>] Block_Size of SIZE:Size
    | [<AltCommandLine("-B")>] Ignore_Backups
    | [<AltCommandLine("-C"); EqualsAssignment>] Color of WHEN:ColorWhen option
    | [<AltCommandLine("-d")>] Directory
    | [<AltCommandLine("-D")>] Dired
    | [<CliPrefix(CliPrefix.Dash)>] F
    | [<AltCommandLine("-F", "--classify", "--file-type"); EqualsAssignment>] Format of WORD:string option
    | [<AltCommandLine("-g")>] Group_Directories_First
    | [<AltCommandLine("-G")>] No_Group
    | [<AltCommandLine("-h")>] Human_Readable
    | [<AltCommandLine("-i")>] INode
    | [<AltCommandLine("-I")>] Ignore of PATTERN:string
    | [<AltCommandLine("-k")>] KibiBytes
    | [<CliPrefix(CliPrefix.Dash)>] L
    | [<AltCommandLine("-L")>] Dereference
    | [<CliPrefix(CliPrefix.Dash)>] M
    | [<AltCommandLine("-n")>] Numeric_Uid_Guid
    | [<AltCommandLine("-N")>] Literal
    | [<CliPrefix(CliPrefix.Dash)>] O
    | [<AltCommandLine("-p"); EqualsAssignment>] Indicator_Style of slash:char
    | [<AltCommandLine("-q")>] Hide_Control_Chars
    | Show_Control_Chars
    | [<AltCommandLine("-Q")>] Quote_Name
    | [<EqualsAssignment>] Quoting_Style of WORD:QuotingStyle
    | [<AltCommandLine("-r")>] Reverse
    | [<AltCommandLine("-R")>] Recursive
    | [<AltCommandLine("-s")>] Size
    | [<CustomCommandLine("-S")>] S
    | [<CliPrefix(CliPrefix.Dash)>] T
    | [<AltCommandLine("-T")>] TabSize of COLS:int
    | [<CliPrefix(CliPrefix.Dash)>] U
    | [<CliPrefix(CliPrefix.Dash)>] V
    | [<AltCommandLine("-w")>] Width of COLS:int
    | [<CustomCommandLine("-x")>] List_By_Lines
    | [<CustomCommandLine("-X")>] Sort_By_Entry
    | [<AltCommandLine("-Z")>] Context
    | [<CustomCommandLine("-1")>] List_One
    | Version
with
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | All -> "do not ignore entries starting with ."
            | Almost_All -> "do not list implied . and .."
            | Author -> "with -l, print the author of each file"
            | Escape -> "print C-style escapes for nongraphic characters"
            | Block_Size _ -> "scale sizes by SIZE before printing them; e.g.,\n'--block-size=M' prints sizes in units of\n 1,048,576 bytes"
            | Ignore_Backups -> "do not list implied entries ending with ~"
            | Color _ -> "colorize the output; WHEN can be 'always' (default\nif omitted), 'auto', or 'never'"
            | Directory -> "list directories themselves, not their contents"
            | Dired _ -> "generate output designed for Emacs' dired mode"
            | F -> "do not sort, enable -aU, disable -ls --color"
            | Format _ -> "append indicator (one of */=>@|) to entries"
            | Group_Directories_First -> "group directories before files;\ncan be augmented with a --sort option, but any\nuse of --sort=none (-U) disables grouping"
            | No_Group -> "in a long listing, don't print group names"
            | Human_Readable -> "with -l and/or -s, print human readable sizes\n(e.g., 1K 234M 2G)"
            | INode -> "print the index number of each file"
            | Ignore _ -> "do not list implied entries matching shell PATTERN"
            | KibiBytes -> "default to 1024-byte blocks for disk usage"
            | L -> "use a long listing format"
            | Dereference -> "when showing file information for a symbolic\nlink, show information for the file the link\nreferences rather than for the link itself"
            | M -> "fill width with a comma separated list of entries"
            | Numeric_Uid_Guid -> "like -l, but list numeric user and group IDs"
            | Literal -> "print raw entry names (don't treat e.g. control\ncharacters specially)"
            | O -> "like -l, but do not list group information"
            | Indicator_Style _ -> "append / indicator to directories"
            | Hide_Control_Chars -> "print ? instead of nongraphic characters"
            | Show_Control_Chars -> "show nongraphic characters as-is (the default,\nunless program is 'ls' and output is a terminal)"
            | Quote_Name -> "enclose entry names in double quotes"
            | Quoting_Style _ -> "use quoting style for entry names"
            | Reverse -> "reverse order while sorting"
            | Recursive -> "list subdirectories recursively"
            | Size -> "print the allocated size of each file, in blocks"
            | S -> "sort by file size, largest first"
            | T -> "sort by modification time, newest first"
            | TabSize _ -> "assume tab stops at each COLS instead of 8"
            | U -> "with -lt: sort by, and show, access time;\nwith -l: show access time and sort by name;\notherwise: sort by access time, newest first"
            | V -> "natural sort of (version) numbers within text"
            | Width _ -> "set output width to COLS. 0 means no limit"
            | Context -> "print any security context of each file"
            | List_One -> "list one file per line.  Avoid '\n' with -q or -b"
            | List_By_Lines -> "list entries by lines instead of by columns"
            | Sort_By_Entry -> "sort alphabetically by entry extension"
            | Version -> "output version information and exit"
            | Files _ -> "File expression to list"