from Actions import Text
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Text import EmacsText
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
import keyword

@registerRule
class Python(EmacsBase):
    majorMode = "python-mode"

    mapping = {
        "align dic"   : Cmd("(align-dict)"),
        "align list"  : Cmd("(align-list)"),
        "mark block"  : Cmd("(er/mark-python-block)"),
        "mark state"  : Cmd("(er/mark-python-statement)"),
        "send funk"   : Key("ca-x"),
        "send buff"   : Key("c-c,c-c"),
        "send region" : Key("c-c,c-r"),
        "shell"       : Key("c-c,c-z"),
    }

    keywords = [
        "True", "False", "set", "list", "dict", "None", "self", "print", "object",
        "len", "reversed", "enumerate", "range", ["__init__", "init"],
    ] + keyword.kwlist
