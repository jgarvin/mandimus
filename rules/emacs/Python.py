from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import makeKeywordRule

import keyword

_mapping = {
    "align dic"   : Cmd("(align-dict)"),
    "align list"  : Cmd("(align-list)"),
    "mark block"  : Cmd("(er/mark-python-block)"),
    "mark state"  : Cmd("(er/mark-python-statement)"),
    "send funk"   : Key("ca-x"),
    "send buff"   : Key("c-c,c-c"),
    "send region" : Key("c-c,c-r"),
    "shell"       : Key("c-c,c-z"),
}

PythonRule = makeContextualRule("Python", _mapping, emacsExtras, emacsDefaults)
PythonRule.context.addRequirement(IsEmacs)
PythonRule.context.addRequirement(ModeRequirement(modes=["python-mode", "inferior-python-mode"]))

keywords = [
    "True", "False", "set", "list", "dict", "None", "self", "print", "object",
    "len", "reversed", "enumerate", "range", ["__init__", "init"], "help"
] + keyword.kwlist

PythonKeywordRule = makeKeywordRule(["python-mode", "inferior-python-mode"], keywords)
