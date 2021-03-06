from Actions import Key, Repeat
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule
from rules.emacs.Text import EmacsText

_keywords = [
    "ack",
    ["acore", "A core"],
    ["apt-get", "apt get"],
    ["awk", "ock"],
    "basename",
    "bash",
    "cat",
    ["cd", "CD"],
    #["cd", "see D"],
    ["cp", "copy"],
    "cut",
    "date",
    ["/dev/null", "dev null"],
    "diff",
    "dirname",
    "disown",
    ["dmesg", "D message"],
    "do",
    "done",
    "echo",
    "else",
    "env",
    "exec",
    "exit",
    "export",
    "extract",
    "false",
    ["fi", "fee"],
    "find",
    "force",
    ["g++", "G plus plus"],
    ["gcc", "GCC"],
    "git",
    "git rebase",
    "git pull",
    "git push",
    "grep",
    "head",
    ["--help", "help"],
    "history",
    ["~", "home"],
    "hostname",
    "if",
    "install",
    ["ip", "I P"],
    "jobs",
    "localhost",
    ["ls", "list"],
    "kill",
    "make",
    ["mkdir", "make dir"],
    "mount",
    ["mv", "move"],
    ["nc", "net cat"],
    "ping",
    ["pkill", "P kill"],
    "python",
    "read",
    ["rm", "remove"],
    ["rsync", "R sync"],
    "search",
    ["sed", "said"],
    "set",
    "setopt",
    "shift",
    "sort",
    "source",
    ["ssh", "S S H"],
    ["strace", "S trace"],
    "sudo",
    "tail",
    "tar",
    "tee",
    "then",
    "touch",
    "true",
    ["tsk", "tisk"],
    "type",
    ["uniq", "unique"],
    "unfunction",
    "unset",
    "unsetopt",
    ["/usr/bin/", "user bin"],
    "up",
    "valgrind",
    "wait",
    ["wc", "word count"],
    "which",
    "while",
    ["xargs", "X args"],
    ["zsh", "Z shell"],
    "crontab",
    "sleep",
    "time",
    [">", "stood out"],
    ["2>", "stood err"],
    ["&>", "stood both"],
    ["ulimit", "you limit"],
    ["taskset", "task set"]
]

ShellKeywordRule = KeywordRule(["shell-mode", "sh-mode"], _keywords)

_mapping = {
    "back [<i>]"        : Key("b,enter") * Repeat(extra="i"),
    "forward [<i>]"     : Key("f,enter") * Repeat(extra="i"),
    "surface [<i>]"      : (EmacsText("up") + Key("enter")) * Repeat(extra="i"),
}

ShellRule = makeContextualRule("Shell", _mapping, emacsExtras, emacsDefaults)
ShellRule.context.addRequirement(IsEmacs)
ShellRule.context.addRequirement(ModeRequirement(modes=["shell-mode", "sh-mode"]))
