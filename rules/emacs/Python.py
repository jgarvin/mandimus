from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule

@registerRule
class Python(SeriesMappingRule):
    mapping = {
        "align dic"  : Cmd("(align-dict)"),
        "align list" : Cmd("(align-list)"),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        out = runEmacsCmd("major-mode").strip()
        return out == "python-mode"
