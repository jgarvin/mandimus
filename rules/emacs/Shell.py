from Actions import Key 
from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Text import EmacsText

@registerRule
class Shell(EmacsBase):
    majorMode = ["shell-mode", "sh-mode"]

    keywords = [
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
        "exec",
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
        ["ls", "list"],
        ["mkdir", "make dir"],
        ["mv", "move"],
        "ping",
        "python",
        ["rm", "remove"],
        ["rsync", "R sync"],
        "search",
        "sort",
        ["ssh", "S S H"],
        "then",
        "true",
        ["tsk", "tisk"],
        "type",
        ["uniq", "unique"],
        "unfunction",
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

    mapping = {
        "hiss"              : Key("a-p"),
        "piss"              : Key("a-n"),
        "history"           : Key("a-r"),
        "back [<n>]"        : Key("b,enter:%(n)d"),
        "forward [<n>]"     : Key("f,enter:%(n)d"),
        "ascend [<n>]"      : EmacsText("up") + Key("enter:%(n)d"),
        "interrupt"         : Key("c-c,c-c"),
        "exit"              : Key("c-d"),
        "prompt up [<n>]"   : Key("c-c,c-p:%(n)d"),
        "prompt down [<n>]" : Key("c-c,c-n:%(n)d"),
    }

