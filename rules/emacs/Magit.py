from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd, getMajorMode
from rules.Rule import registerRule
from Actions import Key, Text

@registerRule
class MagitStatus(SeriesMappingRule):
    mapping = {
        "refresh"      : Key("g"),
        "stage"        : Key("s"),
        "unstage"      : Key("u"),
        "commit"       : Key("c"),
        "done"         : Key("q"),
        "level one"    : Key("a-1"),
        "level two"    : Key("a-2"),
        "level three"  : Key("a-3"),
        "level four"   : Key("a-4"),
        "more context" : Key("plus"),        
        "less context" : Key("hyphen"),
        "discard"      : Key("k"),
        "branch"       : Key("b"),
        "merge"        : Key("m"),
        "diff"         : Key("d"),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        return getMajorMode() == "magit-status-mode"

@registerRule
class MagitKeyMode(SeriesMappingRule):
    mapping = {
        "commit" : Key("c"),
        "extend" : Key("e"),
        "amend"  : Key("a"),
        "reword" : Key("r"),
        "fixup"  : Key("f"),
        "squash" : Key("s"),
        "squash" : Key("s"),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        return getMajorMode() == "magit-key-mode"
