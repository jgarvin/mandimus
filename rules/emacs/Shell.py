from Actions import Key 
from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase

@registerRule
class Shell(EmacsBase):
    majorMode = "shell-mode"

    keywords = [
        ["ls", "list"],
        ["mv", "move"],
        ["rm", "remove"],
        "exec",
        ["~", "home"],
        "grep",
        "git",
        "force",
        ["--help", "help"],
        "cat",
        "which",
        "echo",
        ["xargs", "X args"],
        "find",
        ["tsk", "tisk"],
        "jobs",
        "sort",
        ["uniq", "unique"],
        ["cp", "copy"],
        ["cd", "C D"],
        "if",
        ["fi", "fee"],
        "then",
        "else",
        "while",
        "done",
        "true",
        "false",
        "do",
        ["ssh", "S S H"],
        "search",
        ["wc", "word count"],
        ["rsync", "R sync"],
        "ping",
        ["ip", "I P"],
        "python",
        ["gcc", "GCC"],
        ["g++", "G plus plus"],
    ] 

    mapping = {
        "hiss"      : Key("a-p"),
        "piss"      : Key("a-n"),
        "history"   : Key("a-r"),
        "back"      : Key("b,enter"),
        "forward"   : Key("f,enter"),
        "interrupt" : Key("c-c,c-c"),
        "exit"      : Key("c-d"),
    }

