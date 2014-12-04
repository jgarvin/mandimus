from Actions import Text
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Text import EmacsText
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase

@registerRule
class Python(EmacsBase):
    majorMode = "python-mode"

    mapping = {
        "align dic"  : Cmd("(align-dict)"),
        "align list" : Cmd("(align-list)"),
        "mark block" : Cmd("(er/mark-python-block)"),
        "mark state" : Cmd("(er/mark-python-statement)"),
        "True"       : EmacsText("True", lower=False),
        "False"      : EmacsText("False", lower=False),
    }

import keyword
Python.mapping.update({i : EmacsText("%s" % i, lower=False) for i in keyword.kwlist})

otherMainWords = {"True", "False", "set", "list", "dict", "None", "self", "print", "object"}
Python.mapping.update({i : EmacsText("%s" % i, lower=False) for i in otherMainWords})
