from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Key, Text
from rules.emacs.Base import EmacsBase

@registerRule
class MagitStatus(EmacsBase):
    majorMode = "magit-status-mode"

    mapping = {
        "refresh"       : Key("g"),
        "stage"         : Key("s"),
        "unstage"       : Key("u"),
        "commit"        : Key("c"),
        "done"          : Key("q"),
        "level one"     : Key("a-1"),
        "level two"     : Key("a-2"),
        "level three"   : Key("a-3"),
        "level four"    : Key("a-4"),
        "more context"  : Key("plus"),        
        "less context"  : Key("hyphen"),
        "discard"       : Key("k"),
        "branch"        : Key("b"),
        "merge"         : Key("m"),
        "diff"          : Key("d"),
        "push"          : Key("P"),
        "push remote"   : Key("c-u,P"),
        "remote update" : Key("f"),
        "pull"          : Key("F"),
    }
