from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule
from rules.emacs.Text import EmacsText

_keywords = [
    "cat",
    ["cd", "C D"],
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
    "up",
    ["wc", "word count"],
    "which",
    "while",
    ["xargs", "X args"],
    ["zsh", "zish"],

    [">", "stood out"],
    ["2>", "stood err"],
    ["&>", "stood both"],
    ["|", "pipe"],
] 

ShellKeywordRule = KeywordRule(["shell-mode", "sh-mode"], _keywords)
    
_mapping = {
    "back [<n>]"        : Key("b,enter:%(n)d"),
    "forward [<n>]"     : Key("f,enter:%(n)d"),
    "ascend [<n>]"      : EmacsText("up") + Key("enter:%(n)d"),
}

ShellRule = makeContextualRule("Shell", _mapping, emacsExtras, emacsDefaults)
ShellRule.context.addRequirement(IsEmacs)
ShellRule.context.addRequirement(ModeRequirement(modes=["shell-mode", "sh-mode"]))
