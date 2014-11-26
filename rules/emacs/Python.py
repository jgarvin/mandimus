from Actions import Text
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Text import EmacsText
from rules.emacs.Cmd import runEmacsCmd, Cmd, getMajorMode
from rules.Rule import registerRule

@registerRule
class Python(SeriesMappingRule):
    mapping = {
        "align dic"  : Cmd("(align-dict)"),
        "align list" : Cmd("(align-list)"),
        "mark block" : Cmd("(er/mark-python-block)"),
        "mark state" : Cmd("(er/mark-python-statement)"),
        "True"       : EmacsText("True", lower=False),
        "False"      : EmacsText("False", lower=False),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        return getMajorMode() == "python-mode"

import keyword
Python.mapping.update({i : EmacsText("%s" % i, lower=False) for i in keyword.kwlist})

otherMainWords = {"True", "False", "set", "list", "dict", "None", "self", "print", "object"}
Python.mapping.update({i : EmacsText("%s" % i, lower=False) for i in otherMainWords})
