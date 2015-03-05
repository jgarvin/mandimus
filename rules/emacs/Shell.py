from Actions import Key, Repeat
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule
from rules.emacs.Text import EmacsText

_keywords = [
    "cat",
    ["cd", "CD"],
    #["cd", "see D"],
    ["cp", "copy"],
    "date",
    ["/dev/null", "dev null"],
    "disown",
    "do",
    "done",
    "echo",
    "else",
    "env",
    "exec",
    "exit",
    "export",
    "false",
    ["fi", "fee"],
    "find",
    "force",
    ["g++", "G plus plus"],
    ["gcc", "GCC"],
    "git",
    "grep",
    ["--help", "help"],
    "history",
    ["~", "home"],
    "hostname",
    "if",
    ["ip", "I P"],
    "jobs",
    "localhost",
    ["ls", "list"],
    "kill",
    ["mkdir", "make dir"],
    ["mv", "move"],
    "ping",
    ["pkill", "P kill"],
    "python",
    ["rm", "remove"],
    ["rsync", "R sync"],
    "search",
    "set",
    "setopt",
    "sort",
    ["ssh", "S S H"],
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
    ["wc", "word count"],
    "which",
    "while",
    ["xargs", "X args"],
    ["zsh", "Z shell"],

    [">", "stood out"],
    ["2>", "stood err"],
    ["&>", "stood both"],
    ["|", "pipe"],
] 

ShellKeywordRule = KeywordRule(["shell-mode", "sh-mode"], _keywords)
    
_mapping = {
    "back [<i>]"        : Key("b,enter") * Repeat(extra="n"),
    "forward [<i>]"     : Key("f,enter") * Repeat(extra="n"),
    "ascend [<i>]"      : (EmacsText("up") + Key("enter")) * Repeat(extra="n"),
}

ShellRule = makeContextualRule("Shell", _mapping, emacsExtras, emacsDefaults)
ShellRule.context.addRequirement(IsEmacs)
ShellRule.context.addRequirement(ModeRequirement(modes=["shell-mode", "sh-mode"]))
